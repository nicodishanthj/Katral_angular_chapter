// File path: internal/retriever/retriever.go
package retriever

import (
	"context"
	"math"
	"sort"
	"strings"

	ctxbuilder "github.com/nicodishanthj/Katral_phase1/internal/context"
	"github.com/nicodishanthj/Katral_phase1/internal/graph"
	"github.com/nicodishanthj/Katral_phase1/internal/kb"
	"github.com/nicodishanthj/Katral_phase1/internal/kb/model"
	"github.com/nicodishanthj/Katral_phase1/internal/vector"
)

// Embedder describes the minimal contract needed to generate vectors for
// queries against a vector store.
type Embedder = ctxbuilder.Embedder

const (
	// vectorDocLimit controls how many vector matches are considered when
	// enriching a program document. The limit is intentionally modest to
	// balance recall with latency for per-program lookups.
	vectorDocLimit = 20
)

type SearchResult struct {
	Doc   kb.Doc       `json:"doc"`
	Score float64      `json:"score"`
	Graph GraphContext `json:"graph"`
}

type Retriever struct {
	docs        []kb.Doc
	projectDocs map[string][]kb.Doc
	vectors     map[string]map[string]float64
	norms       map[string]float64
	df          map[string]int
	total       int

	graph           graph.Client
	deps            graph.DependencyService
	cache           *graphCache
	graphWeight     float64
	graphDepth      int
	graphRelatedCap int
}

type Option func(*Retriever)

type graphRefresher interface {
	Refresh([]kb.Doc)
}

// GraphContext captures the graph-derived metadata associated with a program.
type GraphContext = ctxbuilder.GraphContext

// WithDependencyService injects a graph dependency service implementation.
func WithDependencyService(service graph.DependencyService) Option {
	return func(r *Retriever) {
		if service == nil {
			r.deps = graph.NoopDependencyService()
		} else {
			r.deps = service
		}
	}
}

// WithCacheSize controls the number of cached graph expansions maintained by
// the retriever.
func WithCacheSize(size int) Option {
	return func(r *Retriever) {
		r.cache = newGraphCache(size)
	}
}

// WithGraphWeight configures the multiplier applied to graph-derived ranking
// signals when blending scores.
func WithGraphWeight(weight float64) Option {
	return func(r *Retriever) {
		r.graphWeight = weight
	}
}

// WithGraphDepth configures the traversal depth used when expanding
// dependencies and impacts.
func WithGraphDepth(depth int) Option {
	return func(r *Retriever) {
		if depth > 0 {
			r.graphDepth = depth
		}
	}
}

// WithRelatedLimit adjusts how many related programs are retrieved for graph
// enrichment.
func WithRelatedLimit(limit int) Option {
	return func(r *Retriever) {
		if limit > 0 {
			r.graphRelatedCap = limit
		}
	}
}

func New(docs []kb.Doc, graphClient graph.Client, opts ...Option) *Retriever {
	r := &Retriever{
		graph:           graphClient,
		deps:            graph.NoopDependencyService(),
		cache:           newGraphCache(128),
		graphWeight:     0.25,
		graphDepth:      3,
		graphRelatedCap: 10,
	}
	for _, opt := range opts {
		if opt != nil {
			opt(r)
		}
	}
	if r.deps == nil {
		r.deps = graph.NoopDependencyService()
	}
	if r.cache == nil {
		r.cache = newGraphCache(128)
	}
	r.Refresh(docs)
	return r
}

// Graph exposes the configured graph client for downstream components.
func (r *Retriever) Graph() graph.Client {
	return r.graph
}

func (r *Retriever) Refresh(docs []kb.Doc) {
	r.rebuildProjectMap(docs)
	r.rebuildIndexes(docs)
	if refresher, ok := r.deps.(graphRefresher); ok {
		refresher.Refresh(r.docs)
	}
}

func (r *Retriever) RefreshProject(projectID string, docs []kb.Doc) {
	normalized := strings.TrimSpace(projectID)
	if normalized == "" {
		r.Refresh(docs)
		return
	}
	if r.projectDocs == nil {
		r.projectDocs = make(map[string][]kb.Doc)
	}
	copied := make([]kb.Doc, len(docs))
	copy(copied, docs)
	r.projectDocs[normalized] = copied
	combined := r.flattenProjectDocs()
	r.rebuildIndexes(combined)
	if refresher, ok := r.deps.(graphRefresher); ok {
		refresher.Refresh(r.docs)
	}
}

func (r *Retriever) rebuildProjectMap(docs []kb.Doc) {
	if len(docs) == 0 {
		r.projectDocs = make(map[string][]kb.Doc)
		r.docs = nil
		return
	}
	mapped := make(map[string][]kb.Doc)
	for _, doc := range docs {
		key := strings.TrimSpace(docProjectID(doc))
		mapped[key] = append(mapped[key], doc)
	}
	r.projectDocs = mapped
}

func (r *Retriever) rebuildIndexes(docs []kb.Doc) {
	r.docs = docs
	r.vectors = make(map[string]map[string]float64)
	r.norms = make(map[string]float64)
	r.df = make(map[string]int)
	r.total = len(docs)
	if r.cache != nil {
		r.cache.Purge()
	}
	for _, doc := range docs {
		corpus := doc.Content + " " + doc.Summary + " " + strings.Join(doc.Technologies, " ")
		terms := tokenize(corpus)
		tf := make(map[string]float64)
		for _, term := range terms {
			tf[term]++
		}
		for term := range tf {
			r.df[term]++
		}
		r.vectors[doc.ID] = tf
	}
	for id, tf := range r.vectors {
		var norm float64
		for term, freq := range tf {
			weight := r.tfidfWeight(term, freq)
			tf[term] = weight
			norm += weight * weight
		}
		r.norms[id] = math.Sqrt(norm)
	}
}

func (r *Retriever) flattenProjectDocs() []kb.Doc {
	if len(r.projectDocs) == 0 {
		return nil
	}
	keys := make([]string, 0, len(r.projectDocs))
	for key := range r.projectDocs {
		keys = append(keys, key)
	}
	sort.Strings(keys)
	var combined []kb.Doc
	for _, key := range keys {
		combined = append(combined, r.projectDocs[key]...)
	}
	return combined
}

func (r *Retriever) docsForProject(projectID string) []kb.Doc {
	normalized := strings.TrimSpace(projectID)
	if normalized == "" {
		return r.docs
	}
	if len(r.projectDocs) > 0 {
		if docs, ok := r.projectDocs[normalized]; ok {
			return docs
		}
		return nil
	}
	var filtered []kb.Doc
	for _, doc := range r.docs {
		if strings.EqualFold(docProjectID(doc), normalized) {
			filtered = append(filtered, doc)
		}
	}
	return filtered
}

func docProjectID(doc kb.Doc) string {
	if trimmed := strings.TrimSpace(doc.ProjectID); trimmed != "" {
		return trimmed
	}
	if doc.Extra != nil {
		if legacy, ok := doc.Extra["project_id"]; ok {
			if trimmed := strings.TrimSpace(legacy); trimmed != "" {
				return trimmed
			}
		}
	}
	return ""
}

func (r *Retriever) Search(query string, limit int) []SearchResult {
	if limit <= 0 {
		limit = 5
	}
	terms := tokenize(query)
	if len(terms) == 0 {
		return nil
	}
	qtf := make(map[string]float64)
	for _, term := range terms {
		qtf[term]++
	}
	var qnorm float64
	for term, freq := range qtf {
		weight := r.tfidfWeight(term, freq)
		qtf[term] = weight
		qnorm += weight * weight
	}
	qnorm = math.Sqrt(qnorm)
	if qnorm == 0 {
		return nil
	}
	scores := make([]SearchResult, 0, len(r.docs))
	cachedQueryContexts := r.cachedQueryContexts(query)
	for _, doc := range r.docs {
		dv := r.vectors[doc.ID]
		if len(dv) == 0 {
			continue
		}
		var dot float64
		for term, weight := range qtf {
			dot += weight * dv[term]
		}
		denom := qnorm * r.norms[doc.ID]
		if denom == 0 {
			continue
		}
		baseScore := dot / denom
		if baseScore <= 0 {
			continue
		}
		ctx := r.graphContext(nil, doc.Program)
		if cached, ok := cachedQueryContexts[normalizeProgram(doc.Program)]; ok {
			ctx = cached
		}
		metric := r.graphMetric(query, ctx)
		ctx.Signal = metric
		finalScore := baseScore * (1 + r.graphWeight*metric)
		scores = append(scores, SearchResult{Doc: doc, Score: finalScore, Graph: ctx})
	}
	sort.Slice(scores, func(i, j int) bool {
		return scores[i].Score > scores[j].Score
	})
	if len(scores) > limit {
		scores = scores[:limit]
	}
	r.storeQueryContexts(query, scores)
	return scores
}

// MatchDocuments adapts the retriever search results to the context builder
// document match format.
func (r *Retriever) MatchDocuments(query string, limit int) []ctxbuilder.DocumentMatch {
	results := r.Search(query, limit)
	matches := make([]ctxbuilder.DocumentMatch, 0, len(results))
	for _, res := range results {
		matches = append(matches, ctxbuilder.DocumentMatch{Doc: res.Doc, Score: res.Score})
	}
	return matches
}

func (r *Retriever) Answer(question string) string {
	results := r.Search(question, 1)
	if len(results) == 0 {
		return "No matching documents found."
	}
	doc := results[0].Doc
	if doc.Summary != "" {
		if len(doc.Technologies) > 0 {
			return doc.Summary + "\nTechnologies: " + strings.Join(doc.Technologies, ", ")
		}
		return doc.Summary
	}
	if doc.Content != "" {
		if len(doc.Technologies) > 0 {
			return doc.Content + "\n\nTechnologies: " + strings.Join(doc.Technologies, ", ")
		}
		return doc.Content
	}
	if len(doc.Technologies) > 0 {
		return "Technologies: " + strings.Join(doc.Technologies, ", ")
	}
	return "No content available."
}

func (r *Retriever) ExplainSymbol(symbol string) string {
	symbol = strings.ToUpper(symbol)
	for _, doc := range r.docs {
		if doc.Program == symbol && doc.Type == "metadata" {
			builder := &strings.Builder{}
			builder.WriteString(doc.Summary)
			if len(doc.Inputs) > 0 {
				builder.WriteString("\nInputs: ")
				builder.WriteString(strings.Join(doc.Inputs, ", "))
			}
			if len(doc.Outputs) > 0 {
				builder.WriteString("\nOutputs: ")
				builder.WriteString(strings.Join(doc.Outputs, ", "))
			}
			if len(doc.Calls) > 0 {
				builder.WriteString("\nCalls: ")
				builder.WriteString(strings.Join(doc.Calls, ", "))
			}
			if len(doc.Paragraphs) > 0 {
				builder.WriteString("\nParagraphs: ")
				builder.WriteString(strings.Join(doc.Paragraphs, ", "))
			}
			if len(doc.Technologies) > 0 {
				builder.WriteString("\nTechnologies: ")
				builder.WriteString(strings.Join(doc.Technologies, ", "))
			}
			return builder.String()
		}
	}
	return "Symbol not found."
}

func (r *Retriever) ProjectDoc(ctx context.Context, projectID string, embedder Embedder, store vector.Store) kb.ProjectDoc {
	docs := r.docsForProject(projectID)
	if len(docs) == 0 {
		return kb.ProjectDoc{}
	}
	programs := aggregateProgramDocs(docs)
	ordered := sortPrograms(programs)
	for idx := range ordered {
		ordered[idx] = r.attachGraph(ctx, ordered[idx])
	}
	if store == nil || embedder == nil {
		return kb.ProjectDoc{Programs: ordered}
	}
	docsByID, metadataByProgram := r.buildDocIndexes(docs)
	ctx = ensureContext(ctx)
	for idx := range ordered {
		ordered[idx] = r.enrichProgramDoc(ctx, ordered[idx], embedder, store, metadataByProgram, docsByID)
	}
	return kb.ProjectDoc{Programs: ordered}
}

func (r *Retriever) ProgramDoc(ctx context.Context, projectID, symbol string, embedder Embedder, store vector.Store) (kb.ProgramDoc, bool) {
	symbol = strings.ToUpper(strings.TrimSpace(symbol))
	if symbol == "" {
		return kb.ProgramDoc{}, false
	}
	docs := r.docsForProject(projectID)
	if len(docs) == 0 {
		return kb.ProgramDoc{}, false
	}
	programs := aggregateProgramDocs(docs)
	base, ok := programs[symbol]
	if !ok {
		return kb.ProgramDoc{}, false
	}
	if store == nil || embedder == nil {
		return *base, true
	}
	docsByID, metadataByProgram := r.buildDocIndexes(docs)
	ctx = ensureContext(ctx)
	enriched := r.enrichProgramDoc(ctx, *base, embedder, store, metadataByProgram, docsByID)
	return enriched, true
}

func (r *Retriever) attachGraph(ctx context.Context, doc kb.ProgramDoc) kb.ProgramDoc {
	ctx = ensureContext(ctx)
	graphCtx := r.graphContext(ctx, doc.Program)
	if len(graphCtx.Dependencies) > 0 {
		doc.Dependencies = graphCtx.Dependencies
	}
	if len(graphCtx.Impacts) > 0 {
		doc.Impacts = graphCtx.Impacts
	}
	if len(graphCtx.Related) > 0 {
		doc.Related = graphCtx.Related
	}
	return doc
}

func (r *Retriever) graphContext(ctx context.Context, program string) GraphContext {
	normalized := normalizeProgram(program)
	if normalized == "" {
		return GraphContext{}
	}
	key := programCacheKey(normalized)
	if r.cache != nil {
		if cached, ok := r.cache.Get(key); ok {
			if gc, ok := cached.(GraphContext); ok {
				return gc
			}
		}
	}
	if r.deps == nil {
		return GraphContext{Program: normalized}
	}
	ctx = ensureContext(ctx)
	deps, _ := r.deps.Dependencies(ctx, normalized, r.graphDepth)
	impacts, _ := r.deps.Impacts(ctx, normalized, r.graphDepth)
	related, _ := r.deps.Related(ctx, normalized, r.graphRelatedCap)
	gc := GraphContext{
		Program:      normalized,
		Dependencies: toGraphNeighbors(deps),
		Impacts:      toGraphNeighbors(impacts),
		Related:      toGraphNeighbors(related),
	}
	if r.cache != nil {
		r.cache.Set(key, gc)
	}
	return gc
}

func (r *Retriever) cachedQueryContexts(query string) map[string]GraphContext {
	result := make(map[string]GraphContext)
	if r.cache == nil {
		return result
	}
	key := queryCacheKey(query)
	cached, ok := r.cache.Get(key)
	if !ok {
		return result
	}
	contexts, ok := cached.([]GraphContext)
	if !ok {
		return result
	}
	for _, ctx := range contexts {
		program := normalizeProgram(ctx.Program)
		if program == "" {
			continue
		}
		result[program] = ctx
	}
	return result
}

func (r *Retriever) storeQueryContexts(query string, results []SearchResult) {
	if r.cache == nil {
		return
	}
	key := queryCacheKey(query)
	contexts := make([]GraphContext, 0, len(results))
	for _, res := range results {
		if res.Graph.Program == "" {
			continue
		}
		contexts = append(contexts, res.Graph)
	}
	if len(contexts) == 0 {
		return
	}
	r.cache.Set(key, contexts)
}

func (r *Retriever) graphMetric(query string, ctx GraphContext) float64 {
	if ctx.Program == "" {
		return 0
	}
	normalizedQuery := strings.ToUpper(strings.TrimSpace(query))
	var matchBoost float64
	consider := [][]kb.GraphNeighbor{ctx.Dependencies, ctx.Impacts, ctx.Related}
	for _, group := range consider {
		for _, neighbor := range group {
			dist := float64(neighbor.Distance)
			if dist <= 0 {
				dist = 1
			}
			weight := 1 / dist
			if neighbor.Weight > 0 {
				weight += neighbor.Weight * 0.1
			}
			if normalizedQuery != "" {
				programKey := strings.ToUpper(strings.TrimSpace(neighbor.Program))
				if programKey != "" && strings.Contains(normalizedQuery, programKey) {
					matchBoost += weight
					continue
				}
				nameKey := strings.ToUpper(strings.TrimSpace(neighbor.Name))
				if nameKey != "" && strings.Contains(normalizedQuery, nameKey) {
					matchBoost += weight * 0.75
				}
			}
		}
	}
	connectivity := math.Log1p(float64(len(ctx.Dependencies) + len(ctx.Impacts) + len(ctx.Related)))
	metric := connectivity + matchBoost
	return metric
}

func toGraphNeighbors(neighbors []graph.Neighbor) []kb.GraphNeighbor {
	if len(neighbors) == 0 {
		return nil
	}
	out := make([]kb.GraphNeighbor, 0, len(neighbors))
	for _, neighbor := range neighbors {
		out = append(out, kb.GraphNeighbor{
			Program:  neighbor.Program,
			Name:     neighbor.Name,
			Source:   neighbor.Source,
			Distance: neighbor.Distance,
			Weight:   neighbor.Weight,
			Chain:    append([]string(nil), neighbor.Chain...),
			Kind:     string(neighbor.Kind),
		})
	}
	return out
}

func programCacheKey(program string) string {
	return "program:" + program
}

func queryCacheKey(query string) string {
	trimmed := strings.ToUpper(strings.TrimSpace(query))
	if trimmed == "" {
		return "query:empty"
	}
	return "query:" + trimmed
}

func tokenize(text string) []string {
	text = strings.ToLower(text)
	replacer := strings.NewReplacer(
		".", " ",
		",", " ",
		"\n", " ",
		"\t", " ",
		":", " ",
		";", " ",
		"-", " ",
		"_", " ",
		"(", " ",
		")", " ",
		"'", " ",
		"\"", " ",
	)
	cleaned := replacer.Replace(text)
	fields := strings.Fields(cleaned)
	return fields
}

func (r *Retriever) tfidfWeight(term string, freq float64) float64 {
	df := float64(r.df[term])
	if df == 0 {
		return 0
	}
	idf := math.Log((float64(r.total)+1)/(df+1)) + 1
	return freq * idf
}

func aggregateProgramDocs(docs []kb.Doc) map[string]*kb.ProgramDoc {
	programs := make(map[string]*kb.ProgramDoc)
	for _, doc := range docs {
		key := strings.ToUpper(strings.TrimSpace(doc.Program))
		if key == "" {
			continue
		}
		program, ok := programs[key]
		if !ok {
			program = &kb.ProgramDoc{Program: key}
			programs[key] = program
		}
		switch doc.Type {
		case "metadata":
			program.Program = key
			program.SourcePath = doc.SourcePath
			program.Technologies = cloneStrings(doc.Technologies)
			program.Inputs = cloneStrings(doc.Inputs)
			program.Outputs = cloneStrings(doc.Outputs)
			program.Calls = cloneStrings(doc.Calls)
			program.Paragraphs = cloneStrings(doc.Paragraphs)
			program.Logic = cloneStrings(doc.Logic)
			program.LogicTree = cloneLogicSteps(doc.LogicTree)
		case "flow":
			program.Flows = append(program.Flows, doc)
		case "working_notes":
			program.WorkingNotes = append(program.WorkingNotes, doc)
		case "business_rules":
			program.BusinessRules = append(program.BusinessRules, doc)
		case "modernization":
			program.Modernization = append(program.Modernization, doc)
		case "chunk":
			program.Chunks = append(program.Chunks, doc)
		case "documentation_summary":
			program.DocumentationSummaries = append(program.DocumentationSummaries, doc)
		case "documentation_cross_reference":
			program.CrossReferenceMaps = append(program.CrossReferenceMaps, doc)
		case "documentation_impact_analysis":
			program.ImpactAssessments = append(program.ImpactAssessments, doc)
		case "conversion_summary":
			program.ConversionSummaries = append(program.ConversionSummaries, doc)
		case "conversion_mapping":
			program.ConversionMappings = append(program.ConversionMappings, doc)
		case "conversion_source":
			program.ConversionSources = append(program.ConversionSources, doc)
		case "documentation_program_flow":
			program.FlowPrompts = append(program.FlowPrompts, doc)
		case "documentation_business_rules":
			program.BusinessRulePrompts = append(program.BusinessRulePrompts, doc)
		case "documentation_functional_spec":
			program.FunctionalPrompts = append(program.FunctionalPrompts, doc)
		case "documentation_technical_spec":
			program.TechnicalPrompts = append(program.TechnicalPrompts, doc)
		case "documentation_migration_assessment":
			program.MigrationAssessments = append(program.MigrationAssessments, doc)
		case "documentation_component_mapping":
			program.ComponentMappings = append(program.ComponentMappings, doc)
		case "documentation_api_compatibility":
			program.APICompatibility = append(program.APICompatibility, doc)
		case "documentation_migration_timeline":
			program.MigrationTimelines = append(program.MigrationTimelines, doc)
		case "documentation_migration_complexity":
			program.MigrationComplexities = append(program.MigrationComplexities, doc)
		case "documentation_pattern_recommendations":
			program.PatternRecommendations = append(program.PatternRecommendations, doc)
		case "documentation_migrated_code_review":
			program.MigratedCodeReviews = append(program.MigratedCodeReviews, doc)
		case "documentation_performance_comparison":
			program.PerformanceComparisons = append(program.PerformanceComparisons, doc)
		case "documentation_side_by_side_comparison":
			program.SideBySideComparisons = append(program.SideBySideComparisons, doc)
		case "documentation_migration_script":
			program.MigrationScripts = append(program.MigrationScripts, doc)
		case "documentation_component_library_mapping":
			program.ComponentLibraryMappings = append(program.ComponentLibraryMappings, doc)
		case "documentation_migration_validation_tests":
			program.MigrationValidationTests = append(program.MigrationValidationTests, doc)
		}
	}
	for _, program := range programs {
		sort.Slice(program.Flows, func(i, j int) bool {
			left := program.Flows[i]
			right := program.Flows[j]
			if left.SourcePath == right.SourcePath {
				if left.Chunk == right.Chunk {
					return left.ID < right.ID
				}
				return left.Chunk < right.Chunk
			}
			return left.SourcePath < right.SourcePath
		})
		sort.Slice(program.WorkingNotes, func(i, j int) bool {
			left := program.WorkingNotes[i]
			right := program.WorkingNotes[j]
			if left.SourcePath == right.SourcePath {
				return left.ID < right.ID
			}
			return left.SourcePath < right.SourcePath
		})
		sort.Slice(program.BusinessRules, func(i, j int) bool {
			left := program.BusinessRules[i]
			right := program.BusinessRules[j]
			if left.SourcePath == right.SourcePath {
				return left.ID < right.ID
			}
			return left.SourcePath < right.SourcePath
		})
		sort.Slice(program.Modernization, func(i, j int) bool {
			left := program.Modernization[i]
			right := program.Modernization[j]
			return left.ID < right.ID
		})
		sort.Slice(program.Chunks, func(i, j int) bool {
			left := program.Chunks[i]
			right := program.Chunks[j]
			if left.SourcePath == right.SourcePath {
				return left.Chunk < right.Chunk
			}
			return left.SourcePath < right.SourcePath
		})
		sort.Slice(program.DocumentationSummaries, func(i, j int) bool {
			left := program.DocumentationSummaries[i]
			right := program.DocumentationSummaries[j]
			if left.SourcePath == right.SourcePath {
				return left.ID < right.ID
			}
			return left.SourcePath < right.SourcePath
		})
		sort.Slice(program.CrossReferenceMaps, func(i, j int) bool {
			left := program.CrossReferenceMaps[i]
			right := program.CrossReferenceMaps[j]
			if left.SourcePath == right.SourcePath {
				return left.ID < right.ID
			}
			return left.SourcePath < right.SourcePath
		})
		sort.Slice(program.ImpactAssessments, func(i, j int) bool {
			left := program.ImpactAssessments[i]
			right := program.ImpactAssessments[j]
			if left.SourcePath == right.SourcePath {
				return left.ID < right.ID
			}
			return left.SourcePath < right.SourcePath
		})
		sort.Slice(program.ConversionSummaries, func(i, j int) bool {
			left := program.ConversionSummaries[i]
			right := program.ConversionSummaries[j]
			if left.SourcePath == right.SourcePath {
				return left.ID < right.ID
			}
			return left.SourcePath < right.SourcePath
		})
		sort.Slice(program.ConversionMappings, func(i, j int) bool {
			left := program.ConversionMappings[i]
			right := program.ConversionMappings[j]
			if left.SourcePath == right.SourcePath {
				return left.ID < right.ID
			}
			return left.SourcePath < right.SourcePath
		})
		sort.Slice(program.ConversionSources, func(i, j int) bool {
			left := program.ConversionSources[i]
			right := program.ConversionSources[j]
			if left.SourcePath == right.SourcePath {
				return left.ID < right.ID
			}
			return left.SourcePath < right.SourcePath
		})
		sort.Slice(program.FlowPrompts, func(i, j int) bool {
			left := program.FlowPrompts[i]
			right := program.FlowPrompts[j]
			if left.SourcePath == right.SourcePath {
				return left.ID < right.ID
			}
			return left.SourcePath < right.SourcePath
		})
		sort.Slice(program.BusinessRulePrompts, func(i, j int) bool {
			left := program.BusinessRulePrompts[i]
			right := program.BusinessRulePrompts[j]
			if left.SourcePath == right.SourcePath {
				return left.ID < right.ID
			}
			return left.SourcePath < right.SourcePath
		})
		sort.Slice(program.FunctionalPrompts, func(i, j int) bool {
			left := program.FunctionalPrompts[i]
			right := program.FunctionalPrompts[j]
			if left.SourcePath == right.SourcePath {
				return left.ID < right.ID
			}
			return left.SourcePath < right.SourcePath
		})
		sort.Slice(program.TechnicalPrompts, func(i, j int) bool {
			left := program.TechnicalPrompts[i]
			right := program.TechnicalPrompts[j]
			if left.SourcePath == right.SourcePath {
				return left.ID < right.ID
			}
			return left.SourcePath < right.SourcePath
		})
		sort.Slice(program.MigrationAssessments, func(i, j int) bool {
			left := program.MigrationAssessments[i]
			right := program.MigrationAssessments[j]
			if left.SourcePath == right.SourcePath {
				return left.ID < right.ID
			}
			return left.SourcePath < right.SourcePath
		})
		sort.Slice(program.ComponentMappings, func(i, j int) bool {
			left := program.ComponentMappings[i]
			right := program.ComponentMappings[j]
			if left.SourcePath == right.SourcePath {
				return left.ID < right.ID
			}
			return left.SourcePath < right.SourcePath
		})
		sort.Slice(program.APICompatibility, func(i, j int) bool {
			left := program.APICompatibility[i]
			right := program.APICompatibility[j]
			if left.SourcePath == right.SourcePath {
				return left.ID < right.ID
			}
			return left.SourcePath < right.SourcePath
		})
		sort.Slice(program.MigrationTimelines, func(i, j int) bool {
			left := program.MigrationTimelines[i]
			right := program.MigrationTimelines[j]
			if left.SourcePath == right.SourcePath {
				return left.ID < right.ID
			}
			return left.SourcePath < right.SourcePath
		})
		sort.Slice(program.MigrationComplexities, func(i, j int) bool {
			left := program.MigrationComplexities[i]
			right := program.MigrationComplexities[j]
			if left.SourcePath == right.SourcePath {
				return left.ID < right.ID
			}
			return left.SourcePath < right.SourcePath
		})
		sort.Slice(program.PatternRecommendations, func(i, j int) bool {
			left := program.PatternRecommendations[i]
			right := program.PatternRecommendations[j]
			if left.SourcePath == right.SourcePath {
				return left.ID < right.ID
			}
			return left.SourcePath < right.SourcePath
		})
		sort.Slice(program.MigratedCodeReviews, func(i, j int) bool {
			left := program.MigratedCodeReviews[i]
			right := program.MigratedCodeReviews[j]
			if left.SourcePath == right.SourcePath {
				return left.ID < right.ID
			}
			return left.SourcePath < right.SourcePath
		})
		sort.Slice(program.PerformanceComparisons, func(i, j int) bool {
			left := program.PerformanceComparisons[i]
			right := program.PerformanceComparisons[j]
			if left.SourcePath == right.SourcePath {
				return left.ID < right.ID
			}
			return left.SourcePath < right.SourcePath
		})
		sort.Slice(program.SideBySideComparisons, func(i, j int) bool {
			left := program.SideBySideComparisons[i]
			right := program.SideBySideComparisons[j]
			if left.SourcePath == right.SourcePath {
				return left.ID < right.ID
			}
			return left.SourcePath < right.SourcePath
		})
		sort.Slice(program.MigrationScripts, func(i, j int) bool {
			left := program.MigrationScripts[i]
			right := program.MigrationScripts[j]
			if left.SourcePath == right.SourcePath {
				return left.ID < right.ID
			}
			return left.SourcePath < right.SourcePath
		})
		sort.Slice(program.ComponentLibraryMappings, func(i, j int) bool {
			left := program.ComponentLibraryMappings[i]
			right := program.ComponentLibraryMappings[j]
			if left.SourcePath == right.SourcePath {
				return left.ID < right.ID
			}
			return left.SourcePath < right.SourcePath
		})
		sort.Slice(program.MigrationValidationTests, func(i, j int) bool {
			left := program.MigrationValidationTests[i]
			right := program.MigrationValidationTests[j]
			if left.SourcePath == right.SourcePath {
				return left.ID < right.ID
			}
			return left.SourcePath < right.SourcePath
		})
	}
	return programs
}

func sortPrograms(programs map[string]*kb.ProgramDoc) []kb.ProgramDoc {
	out := make([]kb.ProgramDoc, 0, len(programs))
	for _, p := range programs {
		out = append(out, *p)
	}
	sort.Slice(out, func(i, j int) bool { return out[i].Program < out[j].Program })
	return out
}

func (r *Retriever) buildDocIndexes(docs []kb.Doc) (map[string]kb.Doc, map[string]kb.Doc) {
	docsByID := make(map[string]kb.Doc, len(docs))
	metadataByProgram := make(map[string]kb.Doc)
	for _, doc := range docs {
		docsByID[doc.ID] = doc
		if strings.EqualFold(strings.TrimSpace(doc.Type), "metadata") {
			metadataByProgram[normalizeProgram(doc.Program)] = doc
		}
	}
	return docsByID, metadataByProgram
}

func (r *Retriever) enrichProgramDoc(ctx context.Context, base kb.ProgramDoc, embedder Embedder, store vector.Store, metadataByProgram, docsByID map[string]kb.Doc) kb.ProgramDoc {
	programKey := normalizeProgram(base.Program)
	query := base.Program
	if meta, ok := metadataByProgram[programKey]; ok {
		if text := strings.TrimSpace(meta.Text()); text != "" {
			query = text
		}
	}
	vectors, err := embedder.Embed(ctx, []string{query})
	if err != nil || len(vectors) == 0 || len(vectors[0]) == 0 {
		return base
	}
	points, err := store.Search(ctx, vectors[0], vectorDocLimit)
	if err != nil || len(points) == 0 {
		return base
	}
	var flows []kb.Doc
	var notes []kb.Doc
	var rules []kb.Doc
	var modernization []kb.Doc
	var chunks []kb.Doc
	var summaries []kb.Doc
	var crossMaps []kb.Doc
	var impacts []kb.Doc
	var conversionSummaries []kb.Doc
	var conversionMappings []kb.Doc
	var conversionSources []kb.Doc
	var programFlows []kb.Doc
	var businessPrompts []kb.Doc
	var functionalPrompts []kb.Doc
	var technicalPrompts []kb.Doc
	var migrationAssessments []kb.Doc
	var componentMappings []kb.Doc
	var apiCompatibility []kb.Doc
	var migrationTimelines []kb.Doc
	var migrationComplexities []kb.Doc
	var patternRecommendations []kb.Doc
	var migratedCodeReviews []kb.Doc
	var performanceComparisons []kb.Doc
	var sideBySideComparisons []kb.Doc
	var migrationScripts []kb.Doc
	var componentLibraryMappings []kb.Doc
	var migrationValidationTests []kb.Doc
	for _, point := range points {
		doc, ok := docsByID[point.ID]
		if !ok {
			continue
		}
		if normalizeProgram(doc.Program) != programKey {
			continue
		}
		switch strings.ToLower(strings.TrimSpace(doc.Type)) {
		case "flow":
			flows = appendIfMissing(flows, doc)
		case "working_notes":
			notes = appendIfMissing(notes, doc)
		case "business_rules":
			rules = appendIfMissing(rules, doc)
		case "modernization":
			modernization = appendIfMissing(modernization, doc)
		case "chunk":
			chunks = appendIfMissing(chunks, doc)
		case "documentation_summary":
			summaries = appendIfMissing(summaries, doc)
		case "documentation_cross_reference":
			crossMaps = appendIfMissing(crossMaps, doc)
		case "documentation_impact_analysis":
			impacts = appendIfMissing(impacts, doc)
		case "conversion_summary":
			conversionSummaries = appendIfMissing(conversionSummaries, doc)
		case "conversion_mapping":
			conversionMappings = appendIfMissing(conversionMappings, doc)
		case "conversion_source":
			conversionSources = appendIfMissing(conversionSources, doc)
		case "documentation_program_flow":
			programFlows = appendIfMissing(programFlows, doc)
		case "documentation_business_rules":
			businessPrompts = appendIfMissing(businessPrompts, doc)
		case "documentation_functional_spec":
			functionalPrompts = appendIfMissing(functionalPrompts, doc)
		case "documentation_technical_spec":
			technicalPrompts = appendIfMissing(technicalPrompts, doc)
		case "documentation_migration_assessment":
			migrationAssessments = appendIfMissing(migrationAssessments, doc)
		case "documentation_component_mapping":
			componentMappings = appendIfMissing(componentMappings, doc)
		case "documentation_api_compatibility":
			apiCompatibility = appendIfMissing(apiCompatibility, doc)
		case "documentation_migration_timeline":
			migrationTimelines = appendIfMissing(migrationTimelines, doc)
		case "documentation_migration_complexity":
			migrationComplexities = appendIfMissing(migrationComplexities, doc)
		case "documentation_pattern_recommendations":
			patternRecommendations = appendIfMissing(patternRecommendations, doc)
		case "documentation_migrated_code_review":
			migratedCodeReviews = appendIfMissing(migratedCodeReviews, doc)
		case "documentation_performance_comparison":
			performanceComparisons = appendIfMissing(performanceComparisons, doc)
		case "documentation_side_by_side_comparison":
			sideBySideComparisons = appendIfMissing(sideBySideComparisons, doc)
		case "documentation_migration_script":
			migrationScripts = appendIfMissing(migrationScripts, doc)
		case "documentation_component_library_mapping":
			componentLibraryMappings = appendIfMissing(componentLibraryMappings, doc)
		case "documentation_migration_validation_tests":
			migrationValidationTests = appendIfMissing(migrationValidationTests, doc)
		}
	}
	enriched := base
	if len(flows) > 0 {
		enriched.Flows = flows
	}
	if len(notes) > 0 {
		enriched.WorkingNotes = notes
	}
	if len(rules) > 0 {
		enriched.BusinessRules = rules
	}
	if len(modernization) > 0 {
		enriched.Modernization = modernization
	}
	if len(chunks) > 0 {
		enriched.Chunks = chunks
	}
	if len(summaries) > 0 {
		enriched.DocumentationSummaries = summaries
	}
	if len(crossMaps) > 0 {
		enriched.CrossReferenceMaps = crossMaps
	}
	if len(impacts) > 0 {
		enriched.ImpactAssessments = impacts
	}
	if len(conversionSummaries) > 0 {
		enriched.ConversionSummaries = conversionSummaries
	}
	if len(conversionMappings) > 0 {
		enriched.ConversionMappings = conversionMappings
	}
	if len(conversionSources) > 0 {
		enriched.ConversionSources = conversionSources
	}
	if len(programFlows) > 0 {
		enriched.FlowPrompts = programFlows
	}
	if len(businessPrompts) > 0 {
		enriched.BusinessRulePrompts = businessPrompts
	}
	if len(functionalPrompts) > 0 {
		enriched.FunctionalPrompts = functionalPrompts
	}
	if len(technicalPrompts) > 0 {
		enriched.TechnicalPrompts = technicalPrompts
	}
	if len(migrationAssessments) > 0 {
		enriched.MigrationAssessments = migrationAssessments
	}
	if len(componentMappings) > 0 {
		enriched.ComponentMappings = componentMappings
	}
	if len(apiCompatibility) > 0 {
		enriched.APICompatibility = apiCompatibility
	}
	if len(migrationTimelines) > 0 {
		enriched.MigrationTimelines = migrationTimelines
	}
	if len(migrationComplexities) > 0 {
		enriched.MigrationComplexities = migrationComplexities
	}
	if len(patternRecommendations) > 0 {
		enriched.PatternRecommendations = patternRecommendations
	}
	if len(migratedCodeReviews) > 0 {
		enriched.MigratedCodeReviews = migratedCodeReviews
	}
	if len(performanceComparisons) > 0 {
		enriched.PerformanceComparisons = performanceComparisons
	}
	if len(sideBySideComparisons) > 0 {
		enriched.SideBySideComparisons = sideBySideComparisons
	}
	if len(migrationScripts) > 0 {
		enriched.MigrationScripts = migrationScripts
	}
	if len(componentLibraryMappings) > 0 {
		enriched.ComponentLibraryMappings = componentLibraryMappings
	}
	if len(migrationValidationTests) > 0 {
		enriched.MigrationValidationTests = migrationValidationTests
	}
	return r.attachGraph(ctx, enriched)
}

func ensureContext(ctx context.Context) context.Context {
	if ctx != nil {
		return ctx
	}
	return context.Background()
}

func appendIfMissing(existing []kb.Doc, doc kb.Doc) []kb.Doc {
	for _, candidate := range existing {
		if candidate.ID == doc.ID {
			return existing
		}
	}
	return append(existing, doc)
}

func normalizeProgram(program string) string {
	return strings.ToUpper(strings.TrimSpace(program))
}

func cloneStrings(values []string) []string {
	if len(values) == 0 {
		return nil
	}
	out := make([]string, len(values))
	copy(out, values)
	return out
}

func cloneLogicSteps(steps []*model.LogicStep) []*model.LogicStep {
	if len(steps) == 0 {
		return nil
	}
	out := make([]*model.LogicStep, 0, len(steps))
	for _, step := range steps {
		if step == nil {
			continue
		}
		cloned := &model.LogicStep{
			Paragraph: step.Paragraph,
			Type:      step.Type,
			Condition: step.Condition,
			Action:    step.Action,
			Raw:       step.Raw,
		}
		if len(step.Targets) > 0 {
			cloned.Targets = append([]string(nil), step.Targets...)
		}
		if len(step.Children) > 0 {
			cloned.Children = cloneLogicSteps(step.Children)
		}
		if len(step.Else) > 0 {
			cloned.Else = cloneLogicSteps(step.Else)
		}
		if len(step.Data) > 0 {
			cloned.Data = make(map[string]string, len(step.Data))
			for k, v := range step.Data {
				cloned.Data[k] = v
			}
		}
		out = append(out, cloned)
	}
	return out
}
