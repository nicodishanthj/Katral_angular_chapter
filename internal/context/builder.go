// File path: internal/context/builder.go
package context

import (
	"context"
	"errors"
	"fmt"
	"math"
	"sort"
	"strings"
	"sync"
	"time"
	"unicode/utf8"

	"github.com/nicodishanthj/Katral_phase1/internal/common"
	"github.com/nicodishanthj/Katral_phase1/internal/common/telemetry"
	"github.com/nicodishanthj/Katral_phase1/internal/graph"
	"github.com/nicodishanthj/Katral_phase1/internal/kb"
	"github.com/nicodishanthj/Katral_phase1/internal/metadata"
	"github.com/nicodishanthj/Katral_phase1/internal/vector"
)

type serviceBuilder struct {
	config        Config
	vector        vector.Store
	metadata      metadata.Store
	documents     DocumentService
	dependencies  graph.DependencyService
	graphCache    *ttlCache
	metadataCache *ttlCache
	vectorCache   *ttlCache
}

// NewBuilder wires the provided backing services into a context builder.
func NewBuilder(cfg Config, store vector.Store, catalog metadata.Store, docs DocumentService, deps graph.DependencyService) (Builder, error) {
	if deps == nil {
		deps = graph.NoopDependencyService()
	}
	if cfg.MaxSnippets <= 0 {
		cfg.MaxSnippets = DefaultConfig().MaxSnippets
	}
	if cfg.MaxSnippetRunes <= 0 {
		cfg.MaxSnippetRunes = DefaultConfig().MaxSnippetRunes
	}
	if cfg.VectorLimit <= 0 {
		cfg.VectorLimit = DefaultConfig().VectorLimit
	}
	if cfg.GraphDepth <= 0 {
		cfg.GraphDepth = DefaultConfig().GraphDepth
	}
	if cfg.GraphRelatedLimit <= 0 {
		cfg.GraphRelatedLimit = DefaultConfig().GraphRelatedLimit
	}
	builder := &serviceBuilder{
		config:        cfg,
		vector:        store,
		metadata:      catalog,
		documents:     docs,
		dependencies:  deps,
		graphCache:    newTTLCache(cfg.GraphCacheTTL),
		metadataCache: newTTLCache(cfg.MetadataCacheTTL),
		vectorCache:   newTTLCache(cfg.VectorCacheTTL),
	}
	return builder, nil
}

func (b *serviceBuilder) GraphDependencies() graph.DependencyService {
	return b.dependencies
}

func (b *serviceBuilder) BuildGoalContext(ctx context.Context, req GoalRequest) (GoalResult, error) {
	result := GoalResult{Query: strings.TrimSpace(req.Query)}
	if result.Query == "" {
		return result, errors.New("query required")
	}
	limit := req.Limit
	if limit <= 0 {
		limit = b.config.MaxSnippets
	}
	snippets := make(map[string]*Snippet)
	var snippetsMu sync.Mutex
	var wg sync.WaitGroup
	logger := common.Logger()
	useVector := true
	if req.UseVector != nil {
		useVector = *req.UseVector
	}
	var vectorUsed bool
	if useVector && b.vector != nil && req.Embedder != nil {
		vectorLimit := b.config.VectorLimit
		if limit > vectorLimit {
			vectorLimit = limit
		}
		if req.Collection != "" {
			b.vector.SetCollection(req.Collection)
		}
		collection := b.vector.Collection()
		if collection != "" {
			result.Collection = collection
		}
		wg.Add(1)
		go func(collection string, limit int) {
			defer wg.Done()
			points, _, err := b.vectorSearch(ctx, result.Query, collection, limit, req.Embedder)
			if err != nil {
				logger.Warn("context: vector search failed", "error", err)
				return
			}
			if len(points) == 0 {
				return
			}
			vectorUsed = true
			for _, pt := range points {
				snippet := b.snippetFromVector(pt)
				if snippet.ID == "" && snippet.Content == "" {
					continue
				}
				snippet.Origin = "vector"
				b.enrichSnippet(ctx, &snippet, req.ProjectID)
				if snippet.Score < b.config.MinScore {
					continue
				}
				key := b.snippetKey(snippet)
				snippetsMu.Lock()
				snippets[key] = b.mergeSnippet(snippets[key], snippet)
				snippetsMu.Unlock()
			}
		}(collection, vectorLimit)
	}
	if b.documents != nil {
		wg.Add(1)
		go func() {
			defer wg.Done()
			fallbackLimit := limit
			if fallbackLimit < b.config.MaxSnippets {
				fallbackLimit = b.config.MaxSnippets
			}
			matches := b.documents.MatchDocuments(result.Query, fallbackLimit)
			for _, match := range matches {
				snippet := b.snippetFromDocument(match)
				if snippet.ID == "" && snippet.Content == "" {
					continue
				}
				snippet.Origin = "document"
				b.enrichSnippet(ctx, &snippet, req.ProjectID)
				if snippet.Score < b.config.MinScore {
					continue
				}
				key := b.snippetKey(snippet)
				snippetsMu.Lock()
				snippets[key] = b.mergeSnippet(snippets[key], snippet)
				if len(snippets) >= limit*2 {
					snippetsMu.Unlock()
					break
				}
				snippetsMu.Unlock()
			}
		}()
	}
	wg.Wait()
	if vectorUsed {
		result.UsedVector = true
	}
	ordered := make([]Snippet, 0, len(snippets))
	for _, snippet := range snippets {
		snippet.Content = b.trimSnippet(snippet.Content)
		ordered = append(ordered, *snippet)
	}
	sort.SliceStable(ordered, func(i, j int) bool {
		if ordered[i].Score == ordered[j].Score {
			return ordered[i].Program < ordered[j].Program
		}
		return ordered[i].Score > ordered[j].Score
	})
	if len(ordered) > limit {
		ordered = ordered[:limit]
	}
	result.Snippets = ordered
	return result, nil
}

func (b *serviceBuilder) BuildProgramContext(ctx context.Context, req ProgramRequest) (ProgramResult, error) {
	result := ProgramResult{Program: strings.TrimSpace(req.Program)}
	if result.Program == "" {
		return result, errors.New("program required")
	}
	if b.documents != nil {
		if doc, ok := b.documents.ProgramDoc(ctx, req.ProjectID, result.Program, req.Embedder, b.vector); ok {
			result.Document = &doc
		}
	}
	result.Metadata = b.lookupMetadata(ctx, req.ProjectID, result.Program)
	result.Graph = b.lookupGraph(ctx, result.Program)
	return result, nil
}

func (b *serviceBuilder) snippetFromVector(pt vector.SearchResult) Snippet {
	snippet := Snippet{ID: pt.ID}
	if snippet.ID == "" {
		snippet.ID = fmt.Sprint(pt.ID)
	}
	snippet.Score = float64(pt.Score) * b.config.VectorWeight
	if pt.Payload != nil {
		if program, ok := pt.Payload["program"].(string); ok {
			snippet.Program = program
		}
		if source, ok := pt.Payload["source"].(string); ok {
			snippet.Source = source
		}
		if summary, ok := pt.Payload["summary"].(string); ok {
			snippet.Summary = summary
		}
		if content, ok := pt.Payload["content"].(string); ok && strings.TrimSpace(content) != "" {
			snippet.Content = content
		}
	}
	return snippet
}

func (b *serviceBuilder) snippetFromDocument(match DocumentMatch) Snippet {
	doc := match.Doc
	snippet := Snippet{
		ID:      doc.ID,
		Program: doc.Program,
		Source:  doc.SourcePath,
		Summary: doc.Summary,
	}
	content := strings.TrimSpace(doc.Content)
	if content == "" {
		content = strings.TrimSpace(doc.Summary)
	}
	snippet.Content = content
	snippet.Score = match.Score * b.config.DocumentWeight
	return snippet
}

func (b *serviceBuilder) enrichSnippet(ctx context.Context, snippet *Snippet, projectID string) {
	if snippet == nil {
		return
	}
	if snippet.Program != "" {
		metadata := b.lookupMetadata(ctx, projectID, snippet.Program)
		if metadata != nil {
			snippet.Metadata = metadata
			snippet.Score += b.metadataSignal(*metadata) * b.config.MetadataWeight
		}
		graphCtx := b.lookupGraph(ctx, snippet.Program)
		if graphCtx.Program != "" {
			snippet.Graph = graphCtx
			snippet.Score += graphCtx.Signal * b.config.GraphWeight
		}
	}
	if snippet.Score < 0 {
		snippet.Score = 0
	}
}

func (b *serviceBuilder) vectorSearch(ctx context.Context, query, collection string, limit int, embedder Embedder) ([]vector.SearchResult, bool, error) {
	if embedder == nil || b.vector == nil {
		return nil, false, errors.New("vector search unavailable")
	}
	key := b.vectorCacheKey(query, collection, limit)
	if cached, ok := b.vectorCache.get(key); ok {
		if results, ok := cached.([]vector.SearchResult); ok {
			clone := append([]vector.SearchResult(nil), results...)
			if limit > 0 && len(clone) > limit {
				clone = clone[:limit]
			}
			telemetry.RecordVectorSearch(true, 0)
			return clone, true, nil
		}
	}
	if strings.TrimSpace(query) == "" {
		return nil, false, nil
	}
	embeddings, err := embedder.Embed(ctx, []string{query})
	if err != nil {
		return nil, false, err
	}
	if len(embeddings) == 0 || len(embeddings[0]) == 0 {
		return nil, false, nil
	}
	points, err := b.vector.Search(ctx, embeddings[0], limit)
	if err != nil {
		return nil, false, err
	}
	if len(points) > 0 {
		stored := append([]vector.SearchResult(nil), points...)
		b.vectorCache.set(key, stored)
	}
	return append([]vector.SearchResult(nil), points...), false, nil
}

func (b *serviceBuilder) vectorCacheKey(query, collection string, limit int) string {
	parts := []string{
		strings.ToUpper(strings.TrimSpace(collection)),
		strings.ToUpper(strings.TrimSpace(query)),
		fmt.Sprintf("%d", limit),
	}
	return strings.Join(parts, "|")
}

func (b *serviceBuilder) lookupMetadata(ctx context.Context, projectID, program string) *metadata.ProgramRecord {
	if b.metadata == nil || strings.TrimSpace(projectID) == "" || strings.TrimSpace(program) == "" {
		return nil
	}
	key := strings.ToUpper(strings.TrimSpace(projectID)) + "|" + strings.ToUpper(strings.TrimSpace(program))
	if cached, ok := b.metadataCache.get(key); ok {
		if rec, ok := cached.(*metadata.ProgramRecord); ok {
			return rec
		}
	}
	opts := metadata.QueryOptions{ProjectID: projectID, NamePattern: program}
	page, err := b.metadata.QueryPrograms(ctx, opts)
	if err != nil {
		common.Logger().Warn("context: metadata query failed", "project", projectID, "program", program, "error", err)
		return nil
	}
	for _, rec := range page.Programs {
		if strings.EqualFold(strings.TrimSpace(rec.Name), strings.TrimSpace(program)) {
			record := rec
			b.metadataCache.set(key, &record)
			return &record
		}
	}
	return nil
}

func (b *serviceBuilder) lookupGraph(ctx context.Context, program string) GraphContext {
	normalized := strings.ToUpper(strings.TrimSpace(program))
	if normalized == "" {
		return GraphContext{}
	}
	if cached, ok := b.graphCache.get(normalized); ok {
		if ctxVal, ok := cached.(GraphContext); ok {
			return ctxVal
		}
	}
	var deps, impacts, related []graph.Neighbor
	var depErr, impErr, relErr error
	logger := common.Logger()
	var wg sync.WaitGroup
	wg.Add(3)
	go func() {
		defer wg.Done()
		start := time.Now()
		deps, depErr = b.dependencies.Dependencies(ctx, program, b.config.GraphDepth)
		telemetry.RecordGraphQuery("dependencies", time.Since(start))
	}()
	go func() {
		defer wg.Done()
		start := time.Now()
		impacts, impErr = b.dependencies.Impacts(ctx, program, b.config.GraphDepth)
		telemetry.RecordGraphQuery("impacts", time.Since(start))
	}()
	go func() {
		defer wg.Done()
		start := time.Now()
		related, relErr = b.dependencies.Related(ctx, program, b.config.GraphRelatedLimit)
		telemetry.RecordGraphQuery("related", time.Since(start))
	}()
	wg.Wait()
	if depErr != nil {
		logger.Warn("context: graph dependency lookup failed", "program", program, "error", depErr)
	}
	if impErr != nil {
		logger.Warn("context: graph impact lookup failed", "program", program, "error", impErr)
	}
	if relErr != nil {
		logger.Warn("context: graph related lookup failed", "program", program, "error", relErr)
	}
	ctxVal := GraphContext{
		Program:      program,
		Dependencies: toGraphNeighbors(deps),
		Impacts:      toGraphNeighbors(impacts),
		Related:      toGraphNeighbors(related),
	}
	ctxVal.Signal = b.graphSignal(ctxVal)
	b.graphCache.set(normalized, ctxVal)
	return ctxVal
}

func (b *serviceBuilder) mergeSnippet(existing *Snippet, incoming Snippet) *Snippet {
	if existing == nil {
		clone := incoming
		return &clone
	}
	if strings.TrimSpace(existing.Content) == "" && strings.TrimSpace(incoming.Content) != "" {
		existing.Content = incoming.Content
	}
	if existing.Score < incoming.Score {
		existing.Score = incoming.Score
	}
	if existing.Metadata == nil && incoming.Metadata != nil {
		existing.Metadata = incoming.Metadata
	}
	if existing.Graph.Program == "" && incoming.Graph.Program != "" {
		existing.Graph = incoming.Graph
	}
	if existing.Origin == "" {
		existing.Origin = incoming.Origin
	}
	return existing
}

func (b *serviceBuilder) snippetKey(snippet Snippet) string {
	parts := []string{
		strings.ToLower(strings.TrimSpace(snippet.ID)),
		strings.ToLower(strings.TrimSpace(snippet.Program)),
		strings.ToLower(strings.TrimSpace(snippet.Source)),
	}
	return strings.Join(parts, "|")
}

func (b *serviceBuilder) trimSnippet(content string) string {
	content = strings.TrimSpace(content)
	if content == "" {
		return content
	}
	limit := b.config.MaxSnippetRunes
	if limit <= 0 {
		return content
	}
	if utf8.RuneCountInString(content) <= limit {
		return content
	}
	runes := []rune(content)
	if len(runes) <= limit {
		return content
	}
	truncated := strings.TrimSpace(string(runes[:limit]))
	if !strings.HasSuffix(truncated, "…") {
		truncated += "…"
	}
	return truncated
}

func (b *serviceBuilder) metadataSignal(record metadata.ProgramRecord) float64 {
	var score float64
	score += math.Log1p(float64(record.DocumentCount))
	score += math.Log1p(float64(record.DocTypeCount)) * 0.5
	if len(record.Technologies) > 0 {
		score += math.Log1p(float64(len(record.Technologies))) * 0.5
	}
	return score
}

func (b *serviceBuilder) graphSignal(ctx GraphContext) float64 {
	total := len(ctx.Dependencies) + len(ctx.Impacts) + len(ctx.Related)
	signal := math.Log1p(float64(total))
	if ctx.Program != "" {
		signal += 0.1
	}
	return signal
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
