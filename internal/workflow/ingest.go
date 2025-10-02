// File path: internal/workflow/ingest.go
package workflow

import (
	"context"
	"fmt"
	"os"
	"sort"
	"strings"
	"sync"
	"time"

	"github.com/nicodishanthj/Katral_phase1/internal/common"
	"github.com/nicodishanthj/Katral_phase1/internal/common/telemetry"
	"github.com/nicodishanthj/Katral_phase1/internal/kb"
	"github.com/nicodishanthj/Katral_phase1/internal/llm"
	"github.com/nicodishanthj/Katral_phase1/internal/vector"
)

type IngestRequest struct {
	Repo       string   `json:"repo"`
	Stacks     []string `json:"stacks"`
	Collection string   `json:"collection"`
	ProjectID  string   `json:"project_id"`
}

type IngestResponse struct {
	Count      int    `json:"count"`
	Collection string `json:"collection,omitempty"`
	Warning    string `json:"warning,omitempty"`
}

func (m *Manager) Ingest(ctx context.Context, req IngestRequest, replace bool) (IngestResponse, error) {
	repo := strings.TrimSpace(req.Repo)
	if repo == "" {
		return IngestResponse{}, fmt.Errorf("repo path required")
	}
	if _, err := os.Stat(repo); err != nil {
		return IngestResponse{}, err
	}
	stacks := make([]string, 0, len(req.Stacks))
	for _, stack := range req.Stacks {
		stack = strings.TrimSpace(stack)
		if stack != "" {
			stacks = append(stacks, stack)
		}
	}
	if len(stacks) == 0 {
		stacks = []string{"cobol"}
	}
	req.Stacks = stacks
	req.Collection = strings.TrimSpace(req.Collection)
	req.ProjectID = strings.TrimSpace(req.ProjectID)
	if req.ProjectID == "" {
		return IngestResponse{}, fmt.Errorf("project id required")
	}
	provider := m.provider
	if provider == nil {
		provider = llm.NewProvider()
	}
	m.AppendLog("info", "Ingesting repository %s", repo)
	indexer := kb.NewIndexer()
	docs, err := indexer.IndexRepo(ctx, repo, stacks)
	if err != nil {
		return IngestResponse{}, err
	}
	if req.ProjectID != "" {
		for idx := range docs {
			docs[idx].ProjectID = req.ProjectID
		}
	}
	existingDocs, err := m.store.AllDocs(ctx, req.ProjectID)
	if err != nil {
		return IngestResponse{}, err
	}
	existingByID := make(map[string]kb.Doc, len(existingDocs))
	for _, doc := range existingDocs {
		if doc.Fingerprint == "" {
			doc.Fingerprint = kb.ComputeFingerprint(doc)
		}
		existingByID[doc.ID] = doc
	}
	filtered := make([]kb.Doc, 0, len(docs))
	unchangedDocs := make([]kb.Doc, 0, len(docs))
	skipped := 0
	for idx := range docs {
		if existing, ok := existingByID[docs[idx].ID]; ok {
			docs[idx].PreviousFingerprint = existing.Fingerprint
			if existing.Summary != "" {
				docs[idx].Summary = existing.Summary
			}
		}
		docs[idx].Fingerprint = kb.ComputeFingerprint(docs[idx])
		if !replace {
			if existing, ok := existingByID[docs[idx].ID]; ok && existing.Fingerprint == docs[idx].Fingerprint {
				skipped++
				unchangedDocs = append(unchangedDocs, docs[idx])
				continue
			}
		}
		filtered = append(filtered, docs[idx])
	}
	if !replace {
		if skipped > 0 {
			m.AppendLog("info", "Skipped %d unchanged documents", skipped)
		}
		docs = filtered
	}
	var warnings []string
	if memErr := telemetry.CheckMemoryBudget("workflow.ingest.prefilter"); memErr != nil {
		warnings = append(warnings, memErr.Error())
		common.Logger().Warn("workflow: memory guard warning", "error", memErr)
	}
	if provider != nil && provider.Name() != "local" {
		enhanced, warn := m.enhanceSummaries(ctx, provider, docs)
		if len(enhanced) > 0 {
			docs = enhanced
		}
		if warn != "" {
			warnings = append(warnings, warn)
		}
	}
	if replace {
		m.AppendLog("info", "Resetting knowledge base before ingesting %s", repo)
		if err := m.store.ReplaceDocs(ctx, req.ProjectID, docs); err != nil {
			return IngestResponse{}, err
		}
	} else {
		if err := m.store.AppendDocs(ctx, req.ProjectID, docs); err != nil {
			return IngestResponse{}, err
		}
	}
	if len(docs) > 0 {
		telemetry.RecordIngestBatch("store", len(docs))
	}
	m.persistMetadataBatch(ctx, req.ProjectID, docs)
	if err := m.refreshRetriever(ctx, req.ProjectID); err != nil {
		return IngestResponse{}, err
	}
	resp := IngestResponse{Count: len(docs)}
	projectCollection := ""
	if req.ProjectID != "" {
		projectCollection = m.collectionForProject(req.ProjectID, req.Collection)
	}
	if projectCollection != "" {
		if m.vector != nil {
			m.vector.SetCollection(projectCollection)
			resp.Collection = m.vector.Collection()
		} else {
			resp.Collection = projectCollection
		}
	}
	providerName := "local"
	if provider != nil {
		providerName = provider.Name()
	}
	if providerName == "local" {
		warnings = append(warnings, "embeddings not generated; OpenAI API key missing")
		if len(warnings) > 0 {
			resp.Warning = strings.Join(warnings, "; ")
			m.AppendLog("warn", resp.Warning)
		}
		m.AppendLog("info", "Indexed %d documents from %s", len(docs), repo)
		return resp, nil
	}
	if m.vector == nil {
		warnings = append(warnings, "vector store unavailable")
		if len(warnings) > 0 {
			resp.Warning = strings.Join(warnings, "; ")
			m.AppendLog("warn", resp.Warning)
		}
		m.AppendLog("info", "Indexed %d documents from %s", len(docs), repo)
		return resp, nil
	}
	type embedCandidate struct {
		doc  kb.Doc
		text string
	}
	embedDocs := make([]kb.Doc, 0, len(docs)+len(unchangedDocs))
	embedDocs = append(embedDocs, docs...)
	embedDocs = append(embedDocs, unchangedDocs...)
	candidates := make([]embedCandidate, 0, len(embedDocs))
	for _, doc := range embedDocs {
		if !m.shouldEmbedDoc(doc) {
			continue
		}
		text := doc.Text()
		if text == "" {
			continue
		}
		candidates = append(candidates, embedCandidate{doc: doc, text: text})
	}
	if len(candidates) == 0 {
		if len(warnings) > 0 {
			resp.Warning = strings.Join(warnings, "; ")
			m.AppendLog("warn", resp.Warning)
		}
		m.AppendLog("info", "Indexed %d documents from %s", len(docs), repo)
		return resp, nil
	}

	batchSize := m.ingestBatchSize
	if batchSize <= 0 {
		batchSize = 64
	}
	processed := 0
	collectionEnsured := false
	for start := 0; start < len(candidates); start += batchSize {
		end := start + batchSize
		if end > len(candidates) {
			end = len(candidates)
		}
		batch := candidates[start:end]
		inputs := make([]string, len(batch))
		docsBatch := make([]kb.Doc, len(batch))
		for i, cand := range batch {
			inputs[i] = cand.text
			docsBatch[i] = cand.doc
		}
		if memErr := telemetry.CheckMemoryBudget("workflow.ingest.embed"); memErr != nil {
			warnings = append(warnings, memErr.Error())
			common.Logger().Warn("workflow: memory guard warning", "error", memErr)
			break
		}
		vectors, err := provider.Embed(ctx, inputs)
		if err != nil {
			warnings = append(warnings, fmt.Sprintf("embedding failed: %v", err))
			m.AppendLog("warn", warnings[len(warnings)-1])
			break
		}
		if len(vectors) == 0 {
			continue
		}
		if !collectionEnsured {
			if err := m.vector.EnsureCollection(ctx, vector.VectorDimension(vectors)); err != nil {
				warnings = append(warnings, fmt.Sprintf("collection ensure failed: %v", err))
				m.AppendLog("warn", warnings[len(warnings)-1])
				break
			}
			collectionEnsured = true
		}
		if err := m.vector.UpsertDocs(ctx, docsBatch, vectors); err != nil {
			warnings = append(warnings, fmt.Sprintf("vector upsert failed: %v", err))
			m.AppendLog("warn", warnings[len(warnings)-1])
			continue
		}
		telemetry.RecordIngestBatch("vector", len(docsBatch))
		processed += len(docsBatch)
	}
	if processed == 0 {
		if len(warnings) > 0 {
			resp.Warning = strings.Join(warnings, "; ")
			m.AppendLog("warn", resp.Warning)
		}
		m.AppendLog("info", "Indexed %d documents from %s", len(docs), repo)
		return resp, nil
	}
	if len(warnings) > 0 {
		resp.Warning = strings.Join(warnings, "; ")
		m.AppendLog("warn", resp.Warning)
	}
	m.AppendLog("info", "Indexed %d documents from %s", len(docs), repo)
	return resp, nil
}

type metadataDocPersister interface {
	PersistDocs(context.Context, string, []kb.Doc) error
}

func (m *Manager) persistMetadataBatch(ctx context.Context, projectID string, docs []kb.Doc) {
	if m.metadata == nil || len(docs) == 0 {
		return
	}
	persister, ok := m.metadata.(metadataDocPersister)
	if !ok {
		return
	}
	batchSize := m.ingestBatchSize
	if batchSize <= 0 {
		batchSize = 128
	}
	for start := 0; start < len(docs); start += batchSize {
		end := start + batchSize
		if end > len(docs) {
			end = len(docs)
		}
		if err := persister.PersistDocs(ctx, projectID, docs[start:end]); err != nil {
			common.Logger().Warn("workflow: metadata persist failed", "error", err, "project", projectID)
			return
		}
		telemetry.RecordIngestBatch("metadata", end-start)
	}
}

func (m *Manager) enhanceSummaries(ctx context.Context, provider llm.Provider, docs []kb.Doc) ([]kb.Doc, string) {
	logger := common.Logger()
	type summaryJob struct {
		index int
		doc   kb.Doc
	}
	type summaryResult struct {
		index   int
		summary string
		err     error
	}
	var jobs []summaryJob
	for idx, doc := range docs {
		if m.shouldRewriteSummary(doc) {
			jobs = append(jobs, summaryJob{index: idx, doc: doc})
		}
	}
	if len(jobs) == 0 {
		logger.Debug("api: no summaries to enhance")
		return docs, ""
	}
	logger.Info("api: enhancing summaries", "count", len(jobs))
	workerCount := min(4, len(jobs))
	jobCh := make(chan summaryJob)
	results := make(chan summaryResult, len(jobs))
	var wg sync.WaitGroup
	for i := 0; i < workerCount; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for job := range jobCh {
				if ctx != nil {
					select {
					case <-ctx.Done():
						results <- summaryResult{index: job.index, err: ctx.Err()}
						continue
					default:
					}
				}
				parentCtx := ctx
				if parentCtx == nil {
					parentCtx = context.Background()
				}
				childCtx, cancel := context.WithTimeout(parentCtx, 30*time.Second)
				messages := buildSummaryMessages(job.doc)
				resp, err := provider.Chat(childCtx, messages)
				cancel()
				if err != nil {
					logger.Error("api: summary generation failed", "error", err)
					results <- summaryResult{index: job.index, err: err}
					continue
				}
				results <- summaryResult{index: job.index, summary: strings.TrimSpace(resp)}
			}
		}()
	}
	go func() {
		for _, job := range jobs {
			jobCh <- job
		}
		close(jobCh)
		wg.Wait()
		close(results)
	}()
	updated := make([]kb.Doc, len(docs))
	copy(updated, docs)
	var failCount int
	var firstErr error
	for res := range results {
		if res.err != nil {
			failCount++
			if firstErr == nil {
				firstErr = res.err
			}
			continue
		}
		if res.summary != "" {
			updated[res.index].Summary = res.summary
			updated[res.index].Fingerprint = kb.ComputeFingerprint(updated[res.index])
		}
	}
	if failCount > 0 {
		logger.Warn("api: summary enhancement partial failure", "failures", failCount, "error", firstErr)
		return updated, fmt.Sprintf("LLM summaries failed for %d document(s): %v", failCount, firstErr)
	}
	logger.Info("api: summary enhancement completed", "updated_docs", len(jobs))
	return updated, ""
}

func (m *Manager) refreshRetriever(ctx context.Context, projectID string) error {
	logger := common.Logger()
	trimmed := strings.TrimSpace(projectID)
	logger.Debug("api: refreshing retriever", "project", trimmed)
	if trimmed == "" {
		docs, err := m.store.AllDocs(ctx, "")
		if err != nil {
			logger.Error("api: failed to refresh retriever", "error", err)
			return err
		}
		m.retriever.Refresh(docs)
		logger.Debug("api: retriever refreshed", "docs", len(docs), "scope", "all")
		return nil
	}
	docs, err := m.store.AllDocs(ctx, trimmed)
	if err != nil {
		logger.Error("api: failed to refresh project retriever", "project", trimmed, "error", err)
		return err
	}
	m.retriever.RefreshProject(trimmed, docs)
	logger.Debug("api: retriever project refreshed", "project", trimmed, "docs", len(docs))
	return nil
}

func (m *Manager) shouldRewriteSummary(doc kb.Doc) bool {
	if doc.Summary == "" && strings.TrimSpace(doc.Content) == "" {
		return false
	}
	if doc.PreviousFingerprint != "" && doc.PreviousFingerprint == doc.Fingerprint {
		return false
	}
	if len(m.rewriteDocTypes) == 0 {
		return true
	}
	_, ok := m.rewriteDocTypes[strings.ToLower(strings.TrimSpace(doc.Type))]
	return ok
}

func (m *Manager) shouldEmbedDoc(doc kb.Doc) bool {
	docType := strings.ToLower(strings.TrimSpace(doc.Type))
	switch docType {
	case "metadata", "flow", "working_notes", "business_rules", "modernization", "chunk":
		return true
	}
	if doc.PreviousFingerprint != "" && doc.PreviousFingerprint == doc.Fingerprint {
		return false
	}
	if len(m.embedDocTypes) == 0 {
		return true
	}
	_, ok := m.embedDocTypes[docType]
	return ok
}

func buildSummaryMessages(doc kb.Doc) []llm.Message {
	systemPrompt := "You are a modernization assistant who rewrites structured COBOL metadata into concise natural language." +
		" Produce two or three sentences that explain the program purpose, key inputs/outputs, external calls, and noteworthy business rules." +
		" Keep the tone professional and avoid bullet lists."
	var builder strings.Builder
	if doc.Program != "" {
		builder.WriteString("Program: ")
		builder.WriteString(doc.Program)
		builder.WriteString("\n")
	}
	if doc.Type != "" {
		builder.WriteString("Document type: ")
		builder.WriteString(doc.Type)
		builder.WriteString("\n")
	}
	if doc.SourcePath != "" {
		builder.WriteString("Source path: ")
		builder.WriteString(doc.SourcePath)
		builder.WriteString("\n")
	}
	if doc.Summary != "" {
		builder.WriteString("Existing summary: ")
		builder.WriteString(doc.Summary)
		builder.WriteString("\n")
	}
	if len(doc.Inputs) > 0 {
		builder.WriteString("Inputs: ")
		builder.WriteString(strings.Join(limitValues(doc.Inputs, 8), ", "))
		builder.WriteString("\n")
	}
	if len(doc.Outputs) > 0 {
		builder.WriteString("Outputs: ")
		builder.WriteString(strings.Join(limitValues(doc.Outputs, 8), ", "))
		builder.WriteString("\n")
	}
	if len(doc.Calls) > 0 {
		builder.WriteString("External calls: ")
		builder.WriteString(strings.Join(limitValues(doc.Calls, 8), ", "))
		builder.WriteString("\n")
	}
	if len(doc.Paragraphs) > 0 {
		builder.WriteString("Important paragraphs: ")
		builder.WriteString(strings.Join(limitValues(doc.Paragraphs, 6), ", "))
		builder.WriteString("\n")
	}
	if len(doc.Logic) > 0 {
		builder.WriteString("Logic highlights:\n")
		for _, line := range limitValues(doc.Logic, 6) {
			builder.WriteString("- ")
			builder.WriteString(line)
			builder.WriteString("\n")
		}
	}
	if len(doc.Extra) > 0 {
		keys := make([]string, 0, len(doc.Extra))
		for k := range doc.Extra {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		builder.WriteString("Additional metadata:\n")
		for _, k := range keys {
			if v := strings.TrimSpace(doc.Extra[k]); v != "" {
				builder.WriteString("- ")
				builder.WriteString(k)
				builder.WriteString(": ")
				builder.WriteString(v)
				builder.WriteString("\n")
			}
		}
	}
	if doc.Content != "" {
		builder.WriteString("Supporting content:\n")
		builder.WriteString(trimText(doc.Content, 1200))
		builder.WriteString("\n")
	}
	return []llm.Message{
		{Role: "system", Content: systemPrompt},
		{Role: "user", Content: builder.String()},
	}
}

func limitValues(values []string, limit int) []string {
	if limit <= 0 || len(values) <= limit {
		return values
	}
	return values[:limit]
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func trimText(text string, limit int) string {
	cleaned := strings.TrimSpace(text)
	runes := []rune(cleaned)
	if limit <= 0 || len(runes) <= limit {
		return cleaned
	}
	trimmed := strings.TrimSpace(string(runes[:limit]))
	if trimmed == "" {
		return cleaned
	}
	return trimmed + "…"
}
