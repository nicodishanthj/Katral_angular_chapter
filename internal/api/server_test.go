// File path: internal/api/server_test.go
package api

import (
	"archive/zip"
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"mime/multipart"
	"net/http"
	"net/http/httptest"
	"net/url"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/nicodishanthj/Katral_phase1/internal/common/process"
	"github.com/nicodishanthj/Katral_phase1/internal/data/orchestrator"
	"github.com/nicodishanthj/Katral_phase1/internal/graph"
	"github.com/nicodishanthj/Katral_phase1/internal/kb"
	"github.com/nicodishanthj/Katral_phase1/internal/llm"
	"github.com/nicodishanthj/Katral_phase1/internal/memory"
	"github.com/nicodishanthj/Katral_phase1/internal/vector"
	"github.com/nicodishanthj/Katral_phase1/internal/workflow"
)

type mockProvider struct {
	chatResponse string
	embedVectors [][]float32
	lastMessages []llm.Message
	chatCalls    int
	embedCalls   int
}

func (m *mockProvider) Chat(ctx context.Context, messages []llm.Message) (string, error) {
	m.chatCalls++
	m.lastMessages = append([]llm.Message(nil), messages...)
	if m.chatResponse == "" {
		return "mock-response", nil
	}
	return m.chatResponse, nil
}

func (m *mockProvider) Embed(ctx context.Context, input []string) ([][]float32, error) {
	m.embedCalls++
	if len(input) == 0 {
		return nil, nil
	}
	if len(m.embedVectors) >= len(input) {
		vectors := make([][]float32, len(input))
		for i := range input {
			if i < len(m.embedVectors) {
				vectors[i] = append([]float32(nil), m.embedVectors[i]...)
			}
		}
		return vectors, nil
	}
	vectors := make([][]float32, len(input))
	for i := range input {
		if i < len(m.embedVectors) && len(m.embedVectors[i]) > 0 {
			vectors[i] = append([]float32(nil), m.embedVectors[i]...)
			continue
		}
		vectors[i] = []float32{0.1, 0.2, 0.3}
	}
	return vectors, nil
}

func (m *mockProvider) Name() string {
	return "mock"
}

type fakeVector struct {
	available       bool
	collection      string
	ensureCalls     int
	upsertCount     int
	searchCalled    int
	searchInputs    [][]float32
	searchResults   [][]vector.SearchResult
	searchErr       error
	ensureErr       error
	upsertErr       error
	recoverOnEnsure bool
	recoverOnSearch bool
}

func (f *fakeVector) Available() bool { return f.available }

func (f *fakeVector) SetCollection(name string) { f.collection = name }

func (f *fakeVector) Collection() string { return f.collection }

func (f *fakeVector) EnsureCollection(ctx context.Context, dim int) error {
	f.ensureCalls++
	if f.recoverOnEnsure {
		f.available = true
	}
	if f.ensureErr != nil {
		return f.ensureErr
	}
	return nil
}

func (f *fakeVector) UpsertDocs(ctx context.Context, docs []kb.Doc, vectors [][]float32) error {
	f.upsertCount += len(docs)
	if f.upsertErr != nil {
		return f.upsertErr
	}
	return nil
}

func (f *fakeVector) Search(ctx context.Context, query []float32, limit int) ([]vector.SearchResult, error) {
	f.searchCalled++
	f.searchInputs = append(f.searchInputs, append([]float32(nil), query...))
	if f.recoverOnSearch {
		f.available = true
	}
	if len(f.searchResults) > 0 {
		results := f.searchResults[0]
		f.searchResults = f.searchResults[1:]
		out := make([]vector.SearchResult, len(results))
		copy(out, results)
		return out, nil
	}
	return nil, f.searchErr
}

type stubGraphClient struct {
	available bool
}

func (s *stubGraphClient) Available() bool { return s != nil && s.available }

func (s *stubGraphClient) EnsureSchema(context.Context) error { return nil }

func (s *stubGraphClient) InsertProgram(context.Context, graph.Program) error { return nil }

func (s *stubGraphClient) InsertCall(context.Context, graph.Call) error { return nil }

func (s *stubGraphClient) InsertParagraph(context.Context, graph.Paragraph) error { return nil }

func (s *stubGraphClient) InsertDataFlow(context.Context, graph.DataFlow) error { return nil }

func (s *stubGraphClient) Close() error { return nil }

func newTestOrchestrator(t *testing.T, vec vector.Store, graphClient graph.Client) (*orchestrator.Orchestrator, *memory.Store) {
	t.Helper()
	dir := t.TempDir()
	cfg := orchestrator.Config{
		MemoryPath:     filepath.Join(dir, "docs.jsonl"),
		SQLitePath:     filepath.Join(dir, "catalog.db"),
		SyncInterval:   time.Hour,
		SyncTimeout:    time.Second,
		MaxSyncRetries: 0,
		RetryBackoff:   10 * time.Millisecond,
	}
	opts := []orchestrator.Option{
		orchestrator.WithSyncDisabled(),
		orchestrator.WithVectorStore(vec),
		orchestrator.WithGraphClient(graphClient),
	}
	orch, err := orchestrator.New(context.Background(), cfg, opts...)
	if err != nil {
		t.Fatalf("orchestrator.New: %v", err)
	}
	t.Cleanup(func() { _ = orch.Close() })
	return orch, orch.Memory()
}

func newTestServer(t *testing.T, provider llm.Provider, vec vector.Store, graphClient graph.Client, cfg *Config) (*Server, *memory.Store, *orchestrator.Orchestrator) {
	t.Helper()
	orch, store := newTestOrchestrator(t, vec, graphClient)
	srv, err := NewServer(context.Background(), orch, provider, cfg)
	if err != nil {
		t.Fatalf("new server: %v", err)
	}
	return srv, store, orch
}

func TestHandleSearchFallback(t *testing.T) {
	orch, store := newTestOrchestrator(t, nil, nil)
	doc := kb.Doc{
		ID:         "PROG:metadata",
		Program:    "PROG",
		SourcePath: "prog.cbl",
		Type:       "metadata",
		Summary:    "Example summary for PROG",
		Technologies: []string{
			"COBOL",
		},
	}
	if err := store.AppendDocs(context.Background(), "search-proj", []kb.Doc{doc}); err != nil {
		t.Fatalf("append docs: %v", err)
	}
	provider := &mockProvider{chatResponse: "ok"}
	srv, err := NewServer(context.Background(), orch, provider, nil)
	if err != nil {
		t.Fatalf("new server: %v", err)
	}
	req := httptest.NewRequest(http.MethodGet, "/v1/search?q=example&limit=1", nil)
	rr := httptest.NewRecorder()
	srv.handleSearch(rr, req)
	if rr.Code != http.StatusOK {
		t.Fatalf("unexpected status: %d", rr.Code)
	}
	var resp struct {
		Results    []map[string]interface{} `json:"results"`
		Collection string                   `json:"collection"`
	}
	if err := json.NewDecoder(rr.Body).Decode(&resp); err != nil {
		t.Fatalf("decode: %v", err)
	}
	if len(resp.Results) == 0 {
		t.Fatalf("expected results")
	}
	if resp.Collection != "memory" {
		t.Fatalf("expected memory collection, got %q", resp.Collection)
	}
	payload, ok := resp.Results[0]["payload"].(map[string]interface{})
	if !ok {
		t.Fatalf("payload missing")
	}
	if payload["summary"] != doc.Summary {
		t.Fatalf("unexpected summary: %v", payload["summary"])
	}
}

func TestHandleSearchRecoversVector(t *testing.T) {
	doc := kb.Doc{
		ID:         "DOC1",
		Program:    "PRG001",
		SourcePath: "prog.cbl",
		Summary:    "Vector-backed summary",
	}
	provider := &mockProvider{embedVectors: [][]float32{{0.2, 0.4, 0.6}}}
	vec := &fakeVector{
		available:       false,
		recoverOnSearch: true,
		searchResults: [][]vector.SearchResult{{{
			ID:    doc.ID,
			Score: 0.91,
			Payload: map[string]interface{}{
				"summary": doc.Summary,
			},
		}}},
	}
	orch, store := newTestOrchestrator(t, vec, nil)
	if err := store.AppendDocs(context.Background(), "search-project", []kb.Doc{doc}); err != nil {
		t.Fatalf("append docs: %v", err)
	}
	srv, err := NewServer(context.Background(), orch, provider, nil)
	if err != nil {
		t.Fatalf("new server: %v", err)
	}
	req := httptest.NewRequest(http.MethodGet, "/v1/search?q=example&project_id=search-project&collection=shared", nil)
	rr := httptest.NewRecorder()
	srv.handleSearch(rr, req)
	if rr.Code != http.StatusOK {
		t.Fatalf("unexpected status: %d", rr.Code)
	}
	var resp struct {
		Results    []map[string]interface{} `json:"results"`
		Collection string                   `json:"collection"`
	}
	if err := json.NewDecoder(rr.Body).Decode(&resp); err != nil {
		t.Fatalf("decode: %v", err)
	}
	if vec.searchCalled != 1 {
		t.Fatalf("expected vector search attempt, got %d", vec.searchCalled)
	}
	if len(resp.Results) != 1 {
		t.Fatalf("expected 1 result, got %d", len(resp.Results))
	}
	if resp.Collection != vec.Collection() {
		t.Fatalf("expected response collection %q, got %q", vec.Collection(), resp.Collection)
	}
	if resp.Collection == "memory" {
		t.Fatalf("expected vector collection, got fallback memory")
	}
	if !vec.available {
		t.Fatalf("expected vector client to recover availability")
	}
}

func TestHandleProgramDocUsesVectorResults(t *testing.T) {
	meta := kb.Doc{
		ID:         "prog:metadata",
		Program:    "PROG",
		SourcePath: "prog.cbl",
		ProjectID:  "docs-proj",
		Type:       "metadata",
		Summary:    "Program summary",
	}
	flow := kb.Doc{
		ID:         "prog:flow:0",
		Program:    "PROG",
		SourcePath: "prog.cbl",
		ProjectID:  "docs-proj",
		Type:       "flow",
		Content:    "Flow details",
	}
	rule := kb.Doc{
		ID:         "prog:rules:0",
		Program:    "PROG",
		SourcePath: "prog.cbl",
		ProjectID:  "docs-proj",
		Type:       "business_rules",
		Content:    "Rule details",
	}
	provider := &mockProvider{embedVectors: [][]float32{{0.9, 0.1, 0.2}}}
	vec := &fakeVector{available: true, searchResults: [][]vector.SearchResult{{
		{ID: flow.ID, Score: 0.9},
		{ID: rule.ID, Score: 0.8},
	}}}
	orch, store := newTestOrchestrator(t, vec, nil)
	if err := store.AppendDocs(context.Background(), "docs-proj", []kb.Doc{meta, flow, rule}); err != nil {
		t.Fatalf("append docs: %v", err)
	}
	srv, err := NewServer(context.Background(), orch, provider, nil)
	if err != nil {
		t.Fatalf("new server: %v", err)
	}
	req := httptest.NewRequest(http.MethodGet, "/api/program-doc?symbol=PROG&project_id=docs-proj", nil)
	rr := httptest.NewRecorder()
	srv.handleProgramDoc(rr, req)
	if rr.Code != http.StatusOK {
		t.Fatalf("unexpected status: %d", rr.Code)
	}
	if vec.searchCalled != 1 {
		t.Fatalf("expected vector search to be used, got %d", vec.searchCalled)
	}
	var resp kb.ProgramDoc
	if err := json.NewDecoder(rr.Body).Decode(&resp); err != nil {
		t.Fatalf("decode: %v", err)
	}
	if len(resp.Flows) != 1 || resp.Flows[0].ID != flow.ID {
		t.Fatalf("expected flow doc from vector search, got %+v", resp.Flows)
	}
	if len(resp.BusinessRules) != 1 || resp.BusinessRules[0].ID != rule.ID {
		t.Fatalf("expected business rule doc from vector search, got %+v", resp.BusinessRules)
	}
}

func TestHandleProjectDocFallsBackOnVectorFailure(t *testing.T) {
	meta := kb.Doc{ID: "prog:metadata", Program: "PROG", SourcePath: "prog.cbl", ProjectID: "docs-proj", Type: "metadata", Summary: "Summary"}
	flow := kb.Doc{ID: "prog:flow:0", Program: "PROG", SourcePath: "prog.cbl", ProjectID: "docs-proj", Type: "flow", Content: "Flow"}
	provider := &mockProvider{embedVectors: [][]float32{{0.3, 0.4, 0.5}}}
	vec := &fakeVector{available: true, searchErr: fmt.Errorf("boom")}
	orch, store := newTestOrchestrator(t, vec, nil)
	if err := store.AppendDocs(context.Background(), "docs-proj", []kb.Doc{meta, flow}); err != nil {
		t.Fatalf("append docs: %v", err)
	}
	srv, err := NewServer(context.Background(), orch, provider, nil)
	if err != nil {
		t.Fatalf("new server: %v", err)
	}
	req := httptest.NewRequest(http.MethodGet, "/api/project-doc?project_id=docs-proj", nil)
	rr := httptest.NewRecorder()
	srv.handleProjectDoc(rr, req)
	if rr.Code != http.StatusOK {
		t.Fatalf("unexpected status: %d", rr.Code)
	}
	if vec.searchCalled == 0 {
		t.Fatalf("expected vector search to be attempted")
	}
	var resp kb.ProjectDoc
	if err := json.NewDecoder(rr.Body).Decode(&resp); err != nil {
		t.Fatalf("decode: %v", err)
	}
	if len(resp.Programs) != 1 {
		t.Fatalf("expected one program, got %d", len(resp.Programs))
	}
	if len(resp.Programs[0].Flows) != 1 || resp.Programs[0].Flows[0].ID != flow.ID {
		t.Fatalf("expected fallback flows from aggregator, got %+v", resp.Programs[0].Flows)
	}
}

func TestProjectScopedHandlersRespectProjectID(t *testing.T) {
	orch, store := newTestOrchestrator(t, nil, nil)
	docsA := []kb.Doc{{ID: "a:meta", Program: "PROGA", ProjectID: "proj-a", Type: "metadata"}}
	docsB := []kb.Doc{
		{ID: "b:meta", Program: "PROGB", ProjectID: "proj-b", Type: "metadata"},
		{ID: "b:flow:0", Program: "PROGB", ProjectID: "proj-b", Type: "flow"},
	}
	if err := store.AppendDocs(context.Background(), "proj-a", docsA); err != nil {
		t.Fatalf("append docs: %v", err)
	}
	if err := store.AppendDocs(context.Background(), "proj-b", docsB); err != nil {
		t.Fatalf("append docs: %v", err)
	}
	srv, err := NewServer(context.Background(), orch, nil, nil)
	if err != nil {
		t.Fatalf("new server: %v", err)
	}

	projectReq := httptest.NewRequest(http.MethodGet, "/api/project-doc?project_id=proj-a", nil)
	projectResp := httptest.NewRecorder()
	srv.handleProjectDoc(projectResp, projectReq)
	if projectResp.Code != http.StatusOK {
		t.Fatalf("unexpected project status: %d", projectResp.Code)
	}
	var projectDoc kb.ProjectDoc
	if err := json.NewDecoder(projectResp.Body).Decode(&projectDoc); err != nil {
		t.Fatalf("decode: %v", err)
	}
	if len(projectDoc.Programs) != 1 || projectDoc.Programs[0].Program != "PROGA" {
		t.Fatalf("expected only project A program, got %+v", projectDoc.Programs)
	}

	wrongReq := httptest.NewRequest(http.MethodGet, "/api/program-doc?symbol=PROGB&project_id=proj-a", nil)
	wrongResp := httptest.NewRecorder()
	srv.handleProgramDoc(wrongResp, wrongReq)
	if wrongResp.Code != http.StatusNotFound {
		t.Fatalf("expected not found for wrong project, got %d", wrongResp.Code)
	}

	rightReq := httptest.NewRequest(http.MethodGet, "/api/program-doc?symbol=PROGB&project_id=proj-b", nil)
	rightResp := httptest.NewRecorder()
	srv.handleProgramDoc(rightResp, rightReq)
	if rightResp.Code != http.StatusOK {
		t.Fatalf("expected success for project B, got %d", rightResp.Code)
	}
	var programDoc kb.ProgramDoc
	if err := json.NewDecoder(rightResp.Body).Decode(&programDoc); err != nil {
		t.Fatalf("decode: %v", err)
	}
	if len(programDoc.Flows) != 1 || programDoc.Flows[0].ID != "b:flow:0" {
		t.Fatalf("expected project B flows, got %+v", programDoc.Flows)
	}
}

func TestHandleIngestUploadSuccess(t *testing.T) {
	tmp := t.TempDir()
	repoDir := filepath.Join(tmp, "repo")
	if err := os.Mkdir(repoDir, 0o755); err != nil {
		t.Fatalf("mkdir repo: %v", err)
	}
	uploadRoot := filepath.Join(tmp, "uploads")
	srv, store, _ := newTestServer(t, nil, nil, nil, &Config{UploadRoot: uploadRoot})

	var body bytes.Buffer
	writer := multipart.NewWriter(&body)
	if err := writer.WriteField("repo", repoDir); err != nil {
		t.Fatalf("write repo field: %v", err)
	}
	if err := writer.WriteField("collection", "test-collection"); err != nil {
		t.Fatalf("write collection field: %v", err)
	}
	if err := writer.WriteField("project_id", "project-success"); err != nil {
		t.Fatalf("write project field: %v", err)
	}
	part, err := writer.CreateFormFile("files", "program.cbl")
	if err != nil {
		t.Fatalf("create form file: %v", err)
	}
	cobol := "IDENTIFICATION DIVISION.\nPROGRAM-ID. SAMPLE.\nDATA DIVISION.\nPROCEDURE DIVISION.\n    DISPLAY 'HELLO'.\n"
	if _, err := part.Write([]byte(cobol)); err != nil {
		t.Fatalf("write file contents: %v", err)
	}
	if err := writer.Close(); err != nil {
		t.Fatalf("close writer: %v", err)
	}

	req := httptest.NewRequest(http.MethodPost, "/v1/ingest/upload", &body)
	req.Header.Set("Content-Type", writer.FormDataContentType())
	rr := httptest.NewRecorder()

	srv.handleIngestUpload(rr, req)

	if rr.Code != http.StatusOK {
		t.Fatalf("unexpected status: %d", rr.Code)
	}

	var resp struct {
		Uploaded   int    `json:"uploaded"`
		Repo       string `json:"repo"`
		Workspace  string `json:"workspace"`
		Documents  int    `json:"documents"`
		Collection string `json:"collection"`
		Warning    string `json:"warning"`
	}
	if err := json.NewDecoder(rr.Body).Decode(&resp); err != nil {
		t.Fatalf("decode response: %v", err)
	}
	if resp.Uploaded != 1 {
		t.Fatalf("expected 1 uploaded file, got %d", resp.Uploaded)
	}
	if resp.Repo != repoDir {
		t.Fatalf("unexpected repo in response: %s", resp.Repo)
	}
	if resp.Workspace != "" {
		t.Fatalf("workspace should be empty when repo provided")
	}
	if resp.Documents == 0 {
		t.Fatalf("expected documents to be ingested")
	}
	if resp.Collection == "" {
		t.Fatalf("expected collection to be set")
	}

	data, err := os.ReadFile(filepath.Join(repoDir, "program.cbl"))
	if err != nil {
		t.Fatalf("read uploaded file: %v", err)
	}
	if string(data) != cobol {
		t.Fatalf("uploaded file contents mismatch")
	}

	docs, err := store.AllDocs(context.Background(), "project-success")
	if err != nil {
		t.Fatalf("store docs: %v", err)
	}
	if len(docs) == 0 {
		t.Fatalf("expected docs in store after ingest")
	}
	for _, doc := range docs {
		if doc.ProjectID != "project-success" {
			t.Fatalf("expected project_id=project-success, got %q", doc.ProjectID)
		}
	}
}

func TestHandleIngestUploadAllocatesWorkspace(t *testing.T) {
	tmp := t.TempDir()
	uploadRoot := filepath.Join(tmp, "uploads")
	srv, store, _ := newTestServer(t, nil, nil, nil, &Config{UploadRoot: uploadRoot})

	var body bytes.Buffer
	writer := multipart.NewWriter(&body)
	if err := writer.WriteField("collection", "workspace-collection"); err != nil {
		t.Fatalf("write collection field: %v", err)
	}
	if err := writer.WriteField("project_id", "workspace-project"); err != nil {
		t.Fatalf("write project field: %v", err)
	}
	part, err := writer.CreateFormFile("files", "workspace.cbl")
	if err != nil {
		t.Fatalf("create form file: %v", err)
	}
	payload := "IDENTIFICATION DIVISION.\nPROGRAM-ID. WORKSPACE.\nDATA DIVISION.\nPROCEDURE DIVISION.\n    STOP RUN.\n"
	if _, err := part.Write([]byte(payload)); err != nil {
		t.Fatalf("write file contents: %v", err)
	}
	if err := writer.Close(); err != nil {
		t.Fatalf("close writer: %v", err)
	}

	req := httptest.NewRequest(http.MethodPost, "/v1/ingest/upload", &body)
	req.Header.Set("Content-Type", writer.FormDataContentType())
	rr := httptest.NewRecorder()

	srv.handleIngestUpload(rr, req)

	if rr.Code != http.StatusOK {
		t.Fatalf("unexpected status: %d", rr.Code)
	}

	var resp struct {
		Uploaded   int    `json:"uploaded"`
		Repo       string `json:"repo"`
		Workspace  string `json:"workspace"`
		Documents  int    `json:"documents"`
		Collection string `json:"collection"`
		Warning    string `json:"warning"`
	}
	if err := json.NewDecoder(rr.Body).Decode(&resp); err != nil {
		t.Fatalf("decode response: %v", err)
	}
	if resp.Uploaded != 1 {
		t.Fatalf("expected 1 uploaded file, got %d", resp.Uploaded)
	}
	if resp.Repo != "" {
		t.Fatalf("expected repo to be empty when workspace allocated, got %s", resp.Repo)
	}
	if resp.Workspace == "" {
		t.Fatalf("expected workspace identifier in response")
	}
	if strings.Contains(resp.Workspace, string(os.PathSeparator)) {
		t.Fatalf("workspace identifier should not contain path separators: %s", resp.Workspace)
	}
	if resp.Documents == 0 {
		t.Fatalf("expected documents to be ingested")
	}
	if resp.Collection == "" {
		t.Fatalf("expected collection to be set")
	}

	docs, err := store.AllDocs(context.Background(), "workspace-project")
	if err != nil {
		t.Fatalf("store docs: %v", err)
	}
	if len(docs) == 0 {
		t.Fatalf("expected docs in store after ingest")
	}
	for _, doc := range docs {
		if doc.ProjectID != "workspace-project" {
			t.Fatalf("expected project_id=workspace-project, got %q", doc.ProjectID)
		}
	}

	entries, err := os.ReadDir(uploadRoot)
	if err != nil {
		t.Fatalf("read upload root: %v", err)
	}
	if len(entries) != 0 {
		t.Fatalf("expected temporary workspace to be cleaned up, found %d entries", len(entries))
	}
}

func TestHandleProjectsReturnsStoredContexts(t *testing.T) {
	orch, store := newTestOrchestrator(t, nil, nil)
	if err := store.AppendDocs(context.Background(), "proj-alpha", []kb.Doc{{ID: "1"}}); err != nil {
		t.Fatalf("append proj-alpha: %v", err)
	}
	if err := store.AppendDocs(context.Background(), "proj-beta", []kb.Doc{{ID: "2"}, {ID: "3"}}); err != nil {
		t.Fatalf("append proj-beta: %v", err)
	}
	srv, err := NewServer(context.Background(), orch, nil, nil)
	if err != nil {
		t.Fatalf("new server: %v", err)
	}
	req := httptest.NewRequest(http.MethodGet, "/v1/projects", nil)
	rr := httptest.NewRecorder()
	srv.handleProjects(rr, req)
	if rr.Code != http.StatusOK {
		t.Fatalf("unexpected status: %d", rr.Code)
	}
	var resp struct {
		Projects []struct {
			ID        string `json:"project_id"`
			Documents int    `json:"documents"`
		} `json:"projects"`
	}
	if err := json.NewDecoder(rr.Body).Decode(&resp); err != nil {
		t.Fatalf("decode: %v", err)
	}
	if len(resp.Projects) != 2 {
		t.Fatalf("expected 2 projects, got %d", len(resp.Projects))
	}
	counts := make(map[string]int)
	for _, proj := range resp.Projects {
		counts[proj.ID] = proj.Documents
	}
	if counts["proj-alpha"] != 1 || counts["proj-beta"] != 2 {
		t.Fatalf("unexpected project counts: %#v", counts)
	}
}

func TestHandleTechnologies(t *testing.T) {
	tmp := t.TempDir()
	repoDir := filepath.Join(tmp, "repo")
	if err := os.Mkdir(repoDir, 0o755); err != nil {
		t.Fatalf("mkdir repo: %v", err)
	}
	cobol := "IDENTIFICATION DIVISION.\nPROGRAM-ID. SAMPLE.\nDATA DIVISION.\nPROCEDURE DIVISION.\n    DISPLAY 'HELLO'.\n"
	if err := os.WriteFile(filepath.Join(repoDir, "prog.cbl"), []byte(cobol), 0o644); err != nil {
		t.Fatalf("write cobol: %v", err)
	}
	uploadRoot := filepath.Join(tmp, "uploads")
	srv, _, _ := newTestServer(t, nil, nil, nil, &Config{UploadRoot: uploadRoot})

	req := httptest.NewRequest(http.MethodGet, "/v1/technologies?repo="+url.QueryEscape(repoDir), nil)
	rr := httptest.NewRecorder()
	srv.handleTechnologies(rr, req)
	if rr.Code != http.StatusOK {
		t.Fatalf("unexpected status: %d", rr.Code)
	}
	var resp struct {
		Technologies []string `json:"technologies"`
	}
	if err := json.NewDecoder(rr.Body).Decode(&resp); err != nil {
		t.Fatalf("decode: %v", err)
	}
	if len(resp.Technologies) == 0 {
		t.Fatalf("expected technologies in response")
	}
	found := false
	for _, tech := range resp.Technologies {
		if tech == "COBOL" {
			found = true
			break
		}
	}
	if !found {
		t.Fatalf("expected COBOL technology, got %v", resp.Technologies)
	}

	badReq := httptest.NewRequest(http.MethodGet, "/v1/technologies?repo="+url.QueryEscape(filepath.Join(repoDir, "missing")), nil)
	badRR := httptest.NewRecorder()
	srv.handleTechnologies(badRR, badReq)
	if badRR.Code != http.StatusBadRequest {
		t.Fatalf("expected bad request for missing repo, got %d", badRR.Code)
	}
}

func TestHandleIngestUploadValidationErrors(t *testing.T) {
	tmp := t.TempDir()
	repoDir := filepath.Join(tmp, "repo")
	if err := os.Mkdir(repoDir, 0o755); err != nil {
		t.Fatalf("mkdir repo: %v", err)
	}
	srv, _, _ := newTestServer(t, nil, nil, nil, nil)

	buildRequest := func(filename, repo string) *http.Request {
		var body bytes.Buffer
		writer := multipart.NewWriter(&body)
		if repo != "" {
			if err := writer.WriteField("repo", repo); err != nil {
				t.Fatalf("write repo field: %v", err)
			}
		}
		part, err := writer.CreateFormFile("files", filename)
		if err != nil {
			t.Fatalf("create form file: %v", err)
		}
		if _, err := part.Write([]byte("test")); err != nil {
			t.Fatalf("write file contents: %v", err)
		}
		if err := writer.Close(); err != nil {
			t.Fatalf("close writer: %v", err)
		}
		req := httptest.NewRequest(http.MethodPost, "/v1/ingest/upload", &body)
		req.Header.Set("Content-Type", writer.FormDataContentType())
		if err := req.ParseMultipartForm(32 << 20); err != nil {
			t.Fatalf("parse multipart form: %v", err)
		}
		if len(req.MultipartForm.File["files"]) > 0 {
			req.MultipartForm.File["files"][0].Filename = filename
		}
		return req
	}

	cases := []struct {
		name     string
		filename string
		repo     string
	}{
		{name: "path traversal", filename: "../evil.txt", repo: repoDir},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			req := buildRequest(tc.filename, tc.repo)
			rr := httptest.NewRecorder()
			srv.handleIngestUpload(rr, req)
			if rr.Code != http.StatusBadRequest {
				t.Fatalf("expected bad request, got %d", rr.Code)
			}
			var payload map[string]string
			if err := json.NewDecoder(rr.Body).Decode(&payload); err != nil {
				t.Fatalf("decode error payload: %v", err)
			}
			if payload["error"] == "" {
				t.Fatalf("expected error message in response")
			}
		})
	}
}

func TestHandleChatRAGFallback(t *testing.T) {
	orch, store := newTestOrchestrator(t, nil, nil)
	doc := kb.Doc{
		ID:         "PROG:metadata",
		Program:    "PROG",
		SourcePath: "prog.cbl",
		Type:       "metadata",
		Summary:    "Program PROG processes accounts",
		Content:    "IDENTIFICATION DIVISION.",
		Technologies: []string{
			"COBOL",
		},
	}
	if err := store.AppendDocs(context.Background(), "api-doc-proj", []kb.Doc{doc}); err != nil {
		t.Fatalf("append docs: %v", err)
	}
	provider := &mockProvider{chatResponse: "answer"}
	srv, err := NewServer(context.Background(), orch, provider, nil)
	if err != nil {
		t.Fatalf("new server: %v", err)
	}
	body := bytes.NewBufferString(`{"prompt":"Tell me about PROG","use_rag":true}`)
	req := httptest.NewRequest(http.MethodPost, "/v1/chat", body)
	req.Header.Set("Content-Type", "application/json")
	rr := httptest.NewRecorder()
	srv.handleChat(rr, req)
	if rr.Code != http.StatusOK {
		t.Fatalf("unexpected status: %d", rr.Code)
	}
	if len(provider.lastMessages) == 0 {
		t.Fatalf("expected chat messages")
	}
	if len(provider.lastMessages) < 2 {
		t.Fatalf("expected system prompt and context messages, got %d", len(provider.lastMessages))
	}
	systemMsg := provider.lastMessages[0].Content
	if !strings.Contains(systemMsg, "You are Katral") {
		t.Fatalf("expected assistant persona in system message: %s", systemMsg)
	}
	contextMsg := provider.lastMessages[1]
	if contextMsg.Role != "system" || !strings.Contains(contextMsg.Content, "Context snippets") {
		t.Fatalf("expected context system message, got %+v", contextMsg)
	}
	var resp struct {
		Context []string `json:"context"`
	}
	if err := json.NewDecoder(rr.Body).Decode(&resp); err != nil {
		t.Fatalf("decode: %v", err)
	}
	if len(resp.Context) == 0 {
		t.Fatalf("expected context snippets")
	}
	if !strings.Contains(resp.Context[0], "[Snippet 1]") {
		t.Fatalf("context not labeled with snippet identifier: %v", resp.Context[0])
	}
}

func TestHandleAgentIncludesTargetConfig(t *testing.T) {
	provider := &mockProvider{chatResponse: "done"}
	srv, _, _ := newTestServer(t, provider, nil, nil, nil)
	body := bytes.NewBufferString(`{"goal":"Plan modernization","flow":"code-conversion","target_config":{"language":"TypeScript","version":"5.4","framework":"Angular","runtime":"Node.js","notes":"Adopt standalone components"}}`)
	req := httptest.NewRequest(http.MethodPost, "/v1/agent/run", body)
	req.Header.Set("Content-Type", "application/json")
	rr := httptest.NewRecorder()
	srv.handleAgent(rr, req)
	if rr.Code != http.StatusOK {
		t.Fatalf("unexpected status: %d", rr.Code)
	}
	if len(provider.lastMessages) == 0 {
		t.Fatalf("expected provider to receive messages")
	}
	system := provider.lastMessages[0].Content
	if !strings.Contains(system, "Active flow: code-conversion") {
		t.Fatalf("expected flow context, got %q", system)
	}
	if !strings.Contains(system, "language=TypeScript") {
		t.Fatalf("expected language in system prompt: %q", system)
	}
	if !strings.Contains(system, "version=5.4") {
		t.Fatalf("expected version in system prompt: %q", system)
	}
	if !strings.Contains(system, "framework=Angular") {
		t.Fatalf("expected framework in system prompt: %q", system)
	}
	if !strings.Contains(system, "runtime=Node.js") {
		t.Fatalf("expected runtime in system prompt: %q", system)
	}
	if !strings.Contains(system, "notes=Adopt standalone components") {
		t.Fatalf("expected notes in system prompt: %q", system)
	}
}

func TestHandleAgentInjectsKnowledgeContext(t *testing.T) {
	orch, store := newTestOrchestrator(t, nil, nil)
	doc := kb.Doc{
		ID:           "DOC1",
		Program:      "OrderComponent",
		SourcePath:   "src/app/order.component.ts",
		Summary:      "Displays order summaries and handles status updates.",
		Content:      "This Angular component retrieves order data via OrderService and renders a material table.",
		Technologies: []string{"Angular"},
	}
	if err := store.AppendDocs(context.Background(), "agent-doc-proj", []kb.Doc{doc}); err != nil {
		t.Fatalf("append docs: %v", err)
	}
	provider := &mockProvider{chatResponse: "ok"}
	srv, err := NewServer(context.Background(), orch, provider, nil)
	if err != nil {
		t.Fatalf("new server: %v", err)
	}
	body := bytes.NewBufferString(`{"goal":"Summarize OrderComponent modernization context","knowledge_config":{"repo":"legacy-order-system","stacks":["Angular"]}}`)
	req := httptest.NewRequest(http.MethodPost, "/v1/agent/run", body)
	req.Header.Set("Content-Type", "application/json")
	rr := httptest.NewRecorder()
	srv.handleAgent(rr, req)
	if rr.Code != http.StatusOK {
		t.Fatalf("unexpected status: %d", rr.Code)
	}
	if len(provider.lastMessages) < 3 {
		t.Fatalf("expected provider to receive system, context, and user messages, got %d", len(provider.lastMessages))
	}
	contextMsg := provider.lastMessages[1]
	if contextMsg.Role != "system" {
		t.Fatalf("expected context message to be system role, got %q", contextMsg.Role)
	}
	if !strings.Contains(contextMsg.Content, "[Snippet 1]") {
		t.Fatalf("expected context snippet label, got %q", contextMsg.Content)
	}
	if !strings.Contains(contextMsg.Content, "OrderComponent") {
		t.Fatalf("expected snippet to include program metadata, got %q", contextMsg.Content)
	}
	if !strings.Contains(contextMsg.Content, "src/app/order.component.ts") {
		t.Fatalf("expected snippet to include source path, got %q", contextMsg.Content)
	}
	userMsg := provider.lastMessages[len(provider.lastMessages)-1]
	if userMsg.Role != "user" {
		t.Fatalf("expected final message to be user goal, got role %q", userMsg.Role)
	}
}

func TestIngestRepoSkipsUnchangedDocs(t *testing.T) {
	tmp := t.TempDir()
	repoDir := filepath.Join(tmp, "repo")
	if err := os.MkdirAll(repoDir, 0o755); err != nil {
		t.Fatalf("mkdir repo: %v", err)
	}
	cobol := "IDENTIFICATION DIVISION.\nPROGRAM-ID. SAMPLE.\nDATA DIVISION.\nPROCEDURE DIVISION.\n    DISPLAY 'HELLO'.\n"
	if err := os.WriteFile(filepath.Join(repoDir, "sample.cbl"), []byte(cobol), 0o644); err != nil {
		t.Fatalf("write source: %v", err)
	}
	provider := &mockProvider{}
	vectorClient := &fakeVector{available: true, collection: "test"}
	orch, _ := newTestOrchestrator(t, vectorClient, nil)
	srv, err := NewServer(context.Background(), orch, provider, nil)
	if err != nil {
		t.Fatalf("new server: %v", err)
	}

	ctx := context.Background()
	req := ingestRequest{Repo: repoDir, ProjectID: "proj-ingest", Collection: "shared"}
	if _, err := srv.ingestRepo(ctx, req, false); err != nil {
		t.Fatalf("first ingest: %v", err)
	}
	if provider.chatCalls == 0 {
		t.Fatalf("expected first ingest to invoke summary rewriting")
	}
	if provider.embedCalls == 0 {
		t.Fatalf("expected first ingest to invoke embeddings")
	}
	initialChats := provider.chatCalls
	initialEmbeds := provider.embedCalls
	initialUpserts := vectorClient.upsertCount

	if _, err := srv.ingestRepo(ctx, req, false); err != nil {
		t.Fatalf("second ingest: %v", err)
	}
	if provider.chatCalls != initialChats {
		t.Fatalf("expected unchanged docs to skip summary rewriting; before=%d after=%d", initialChats, provider.chatCalls)
	}
	if provider.embedCalls <= initialEmbeds {
		t.Fatalf("expected unchanged docs to re-embed vector-backed types; before=%d after=%d", initialEmbeds, provider.embedCalls)
	}
	if vectorClient.upsertCount <= initialUpserts {
		t.Fatalf("expected unchanged docs to refresh vector payloads; before=%d after=%d", initialUpserts, vectorClient.upsertCount)
	}
}

func TestIngestRepoRecoversVectorAvailability(t *testing.T) {
	tmp := t.TempDir()
	repoDir := filepath.Join(tmp, "repo")
	if err := os.MkdirAll(repoDir, 0o755); err != nil {
		t.Fatalf("mkdir repo: %v", err)
	}
	cobol := "IDENTIFICATION DIVISION.\nPROGRAM-ID. SAMPLE.\nDATA DIVISION.\nPROCEDURE DIVISION.\n    DISPLAY 'HELLO'.\n"
	if err := os.WriteFile(filepath.Join(repoDir, "sample.cbl"), []byte(cobol), 0o644); err != nil {
		t.Fatalf("write source: %v", err)
	}
	provider := &mockProvider{embedVectors: [][]float32{{0.5, 0.6, 0.7}}}
	vectorClient := &fakeVector{available: false, recoverOnEnsure: true}
	orch, _ := newTestOrchestrator(t, vectorClient, nil)
	srv, err := NewServer(context.Background(), orch, provider, nil)
	if err != nil {
		t.Fatalf("new server: %v", err)
	}

	ctx := context.Background()
	req := ingestRequest{Repo: repoDir, ProjectID: "proj-recover", Collection: "shared"}
	resp, err := srv.ingestRepo(ctx, req, false)
	if err != nil {
		t.Fatalf("ingest: %v", err)
	}
	if resp.Count == 0 {
		t.Fatalf("expected documents to be ingested")
	}
	if vectorClient.ensureCalls == 0 {
		t.Fatalf("expected ensure collection to be attempted")
	}
	if vectorClient.upsertCount == 0 {
		t.Fatalf("expected vector upsert to occur")
	}
	if !vectorClient.available {
		t.Fatalf("expected vector client to recover availability")
	}
}

func TestUIRoutes(t *testing.T) {
	tmp := t.TempDir()
	_, err := memory.NewStore(filepath.Join(tmp, "docs.jsonl"))
	if err != nil {
		t.Fatalf("new store: %v", err)
	}

	cwd, err := os.Getwd()
	if err != nil {
		t.Fatalf("get wd: %v", err)
	}
	root := filepath.Dir(filepath.Dir(cwd))
	if err := os.Chdir(root); err != nil {
		t.Fatalf("chdir root: %v", err)
	}
	t.Cleanup(func() {
		_ = os.Chdir(cwd)
	})

	srv, _, _ := newTestServer(t, nil, nil, nil, nil)
	if srv == nil {
		t.Fatalf("new server returned nil")
	}

	req := httptest.NewRequest(http.MethodGet, "/ui/", nil)
	rr := httptest.NewRecorder()
	srv.ServeHTTP(rr, req)
	if rr.Code != http.StatusOK {
		t.Fatalf("expected status 200 for /ui/, got %d (location=%q)", rr.Code, rr.Header().Get("Location"))
	}
	body := rr.Body.String()
	if !strings.Contains(body, "Katral Angular Assistant") {
		t.Fatalf("expected UI HTML to contain title, got: %s", body)
	}

	req = httptest.NewRequest(http.MethodGet, "/ui", nil)
	rr = httptest.NewRecorder()
	srv.ServeHTTP(rr, req)
	if rr.Code != http.StatusMovedPermanently {
		t.Fatalf("expected redirect for /ui, got %d", rr.Code)
	}
	location := rr.Header().Get("Location")
	if location != "/ui/" {
		t.Fatalf("expected redirect location /ui/, got %q", location)
	}
}

func TestWorkflowStatusIncludesTargetConfig(t *testing.T) {
	tmp := t.TempDir()
	srv, _, _ := newTestServer(t, nil, nil, nil, nil)

	sourceRepo := filepath.Join(tmp, "angular-app")
	if err := os.MkdirAll(sourceRepo, 0o755); err != nil {
		t.Fatalf("mkdir source repo: %v", err)
	}
	samplePath := filepath.Join(sourceRepo, "README.md")
	if err := os.WriteFile(samplePath, []byte("example"), 0o644); err != nil {
		t.Fatalf("write sample: %v", err)
	}

	payload := map[string]interface{}{
		"project_id":  "proj-123",
		"source_repo": sourceRepo,
		"stacks":      []string{"Angular"},
		"flow":        "code-conversion",
		"target_config": map[string]string{
			"language":  "TypeScript",
			"version":   "5.4",
			"framework": "Angular",
			"runtime":   "Node.js",
			"notes":     "Use standalone components",
		},
	}
	body, err := json.Marshal(payload)
	if err != nil {
		t.Fatalf("marshal payload: %v", err)
	}

	startReq := httptest.NewRequest(http.MethodPost, "/v1/workflow/start", bytes.NewReader(body))
	startReq.Header.Set("Content-Type", "application/json")
	startRR := httptest.NewRecorder()
	srv.handleWorkflowStart(startRR, startReq)
	if startRR.Code != http.StatusAccepted {
		t.Fatalf("expected 202, got %d: %s", startRR.Code, startRR.Body.String())
	}

	deadline := time.Now().Add(2 * time.Second)
	var state workflow.State
	for {
		statusReq := httptest.NewRequest(http.MethodGet, "/v1/workflow/status", nil)
		q := statusReq.URL.Query()
		q.Set("project_id", "proj-123")
		statusReq.URL.RawQuery = q.Encode()
		statusRR := httptest.NewRecorder()
		srv.handleWorkflowStatus(statusRR, statusReq)
		if statusRR.Code != http.StatusOK {
			t.Fatalf("expected 200 from status, got %d: %s", statusRR.Code, statusRR.Body.String())
		}
		if err := json.NewDecoder(statusRR.Body).Decode(&state); err != nil {
			t.Fatalf("decode status: %v", err)
		}
		if state.Request.TargetConfig != nil && len(state.Steps) > 0 && state.Steps[0].Message != "" {
			break
		}
		if time.Now().After(deadline) {
			t.Fatalf("workflow status never populated with target config")
		}
		time.Sleep(25 * time.Millisecond)
	}

	if state.Request.TargetConfig == nil {
		t.Fatalf("expected target config in workflow request")
	}
	cfg := state.Request.TargetConfig
	if cfg.Language != "TypeScript" || cfg.Version != "5.4" || cfg.Framework != "Angular" || cfg.Runtime != "Node.js" || cfg.Notes != "Use standalone components" {
		t.Fatalf("unexpected target config: %+v", cfg)
	}
	expected := "Target: language=TypeScript, version=5.4, framework=Angular, runtime=Node.js, notes=Use standalone components"
	found := false
	for _, step := range state.Steps {
		if strings.Contains(step.Message, expected) {
			found = true
			break
		}
	}
	if !found {
		t.Fatalf("expected step message to include %q, got %+v", expected, state.Steps)
	}
}

func TestWorkflowDownloadReturnsArtifact(t *testing.T) {
	orch, store := newTestOrchestrator(t, nil, nil)
	artifactRoot := filepath.Join(store.Root(), "artifacts")
	if err := os.MkdirAll(artifactRoot, 0o755); err != nil {
		t.Fatalf("mkdir artifacts: %v", err)
	}
	artifactPath := filepath.Join(artifactRoot, "proj-download.zip")
	if err := os.WriteFile(artifactPath, []byte("example"), 0o644); err != nil {
		t.Fatalf("write artifact: %v", err)
	}
	docArtifactPath := filepath.Join(artifactRoot, "proj-download-doc.zip")
	if err := os.WriteFile(docArtifactPath, []byte("doc"), 0o644); err != nil {
		t.Fatalf("write doc artifact: %v", err)
	}
	convArtifactPath := filepath.Join(artifactRoot, "proj-download-conv.zip")
	if err := os.WriteFile(convArtifactPath, []byte("conv"), 0o644); err != nil {
		t.Fatalf("write conversion artifact: %v", err)
	}
	historyPath := filepath.Join(store.Root(), "projects_history.json")
	file, err := os.OpenFile(historyPath, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0o644)
	if err != nil {
		t.Fatalf("open history: %v", err)
	}
	history := map[string]workflow.State{
		"proj-download": {
			Status: "completed",
			ConversionArtifacts: map[string]string{
				"conversion_source":  artifactPath,
				"conversion_summary": convArtifactPath,
			},
			DocumentationArtifacts: map[string]string{"documentation_summary": docArtifactPath},
			Request: workflow.Request{
				ProjectID: "proj-download",
			},
		},
	}
	if err := json.NewEncoder(file).Encode(history); err != nil {
		file.Close()
		t.Fatalf("write history: %v", err)
	}
	if err := file.Close(); err != nil {
		t.Fatalf("close history: %v", err)
	}

	srv, err := NewServer(context.Background(), orch, nil, nil)
	if err != nil {
		t.Fatalf("new server: %v", err)
	}

	req := httptest.NewRequest(http.MethodGet, "/v1/workflow/download", nil)
	q := req.URL.Query()
	q.Set("project_id", "proj-download")
	q.Set("artifact", "conversion_source")
	req.URL.RawQuery = q.Encode()
	rr := httptest.NewRecorder()
	srv.handleWorkflowDownload(rr, req)
	if rr.Code != http.StatusOK {
		t.Fatalf("expected 200, got %d: %s", rr.Code, rr.Body.String())
	}
	if ct := rr.Header().Get("Content-Type"); ct != "application/zip" {
		t.Fatalf("expected content-type application/zip, got %q", ct)
	}
	disposition := rr.Header().Get("Content-Disposition")
	if !strings.Contains(disposition, "proj-download.zip") {
		t.Fatalf("expected disposition to include filename, got %q", disposition)
	}
	if rr.Body.Len() == 0 {
		t.Fatalf("expected response body to contain artifact data")
	}

	docReq := httptest.NewRequest(http.MethodGet, "/v1/workflow/download?project_id=proj-download&artifact=documentation_summary", nil)
	docRR := httptest.NewRecorder()
	srv.handleWorkflowDownload(docRR, docReq)
	if docRR.Code != http.StatusOK {
		t.Fatalf("expected 200 for documentation artifact, got %d: %s", docRR.Code, docRR.Body.String())
	}
	if ct := docRR.Header().Get("Content-Type"); ct != "application/zip" {
		t.Fatalf("expected content-type application/zip for documentation artifact, got %q", ct)
	}
	if !strings.Contains(docRR.Header().Get("Content-Disposition"), "proj-download-doc.zip") {
		t.Fatalf("expected documentation filename in content disposition, got %q", docRR.Header().Get("Content-Disposition"))
	}
	if docRR.Body.Len() == 0 {
		t.Fatalf("expected documentation artifact body data")
	}

	convReq := httptest.NewRequest(http.MethodGet, "/v1/workflow/download?project_id=proj-download&artifact=conversion_summary", nil)
	convRR := httptest.NewRecorder()
	srv.handleWorkflowDownload(convRR, convReq)
	if convRR.Code != http.StatusOK {
		t.Fatalf("expected 200 for conversion artifact, got %d: %s", convRR.Code, convRR.Body.String())
	}
	if ct := convRR.Header().Get("Content-Type"); ct != "application/zip" {
		t.Fatalf("expected content-type application/zip for conversion artifact, got %q", ct)
	}
	if !strings.Contains(convRR.Header().Get("Content-Disposition"), "proj-download-conv.zip") {
		t.Fatalf("expected conversion filename in content disposition, got %q", convRR.Header().Get("Content-Disposition"))
	}
	if convRR.Body.Len() == 0 {
		t.Fatalf("expected conversion artifact body data")
	}

	noArtifactReq := httptest.NewRequest(http.MethodGet, "/v1/workflow/download?project_id=proj-download", nil)
	noArtifactRR := httptest.NewRecorder()
	srv.handleWorkflowDownload(noArtifactRR, noArtifactReq)
	if noArtifactRR.Code != http.StatusBadRequest {
		t.Fatalf("expected 400 when artifact query is missing, got %d", noArtifactRR.Code)
	}

	missingReq := httptest.NewRequest(http.MethodGet, "/v1/workflow/download?project_id=unknown&artifact=conversion_source", nil)
	missingRR := httptest.NewRecorder()
	srv.handleWorkflowDownload(missingRR, missingReq)
	if missingRR.Code != http.StatusNotFound {
		t.Fatalf("expected 404 for missing artifact, got %d", missingRR.Code)
	}
}

func TestConsolidatedDocHandler(t *testing.T) {
	orch, store := newTestOrchestrator(t, nil, nil)
	artifactRoot := filepath.Join(store.Root(), "artifacts")
	if err := os.MkdirAll(artifactRoot, 0o755); err != nil {
		t.Fatalf("mkdir artifacts: %v", err)
	}
	artifactPath := filepath.Join(artifactRoot, "proj-consolidated.zip")
	file, err := os.Create(artifactPath)
	if err != nil {
		t.Fatalf("create artifact: %v", err)
	}
	writer := zip.NewWriter(file)
	manifest := workflow.ConsolidatedDoc{
		ProjectID:     "proj-consolidated",
		GeneratedAt:   time.Date(2024, time.January, 2, 15, 4, 5, 0, time.UTC),
		DocumentCount: 1,
		TypeCounts:    map[string]int{"documentation_summary": 1},
		Documents: []workflow.ConsolidatedDocumentInfo{{
			ID:      "DOC-1",
			Type:    "documentation_summary",
			Title:   "Overview",
			Program: "PGM",
			Summary: "Summary",
		}},
	}
	manifestPayload, err := json.Marshal(manifest)
	if err != nil {
		t.Fatalf("marshal manifest: %v", err)
	}
	manifestWriter, err := writer.Create("manifest.json")
	if err != nil {
		t.Fatalf("create manifest entry: %v", err)
	}
	if _, err := manifestWriter.Write(manifestPayload); err != nil {
		t.Fatalf("write manifest: %v", err)
	}
	docWriter, err := writer.Create("documentation.md")
	if err != nil {
		t.Fatalf("create markdown entry: %v", err)
	}
	if _, err := docWriter.Write([]byte("# Overview\n")); err != nil {
		t.Fatalf("write markdown: %v", err)
	}
	if err := writer.Close(); err != nil {
		t.Fatalf("close archive: %v", err)
	}
	if err := file.Close(); err != nil {
		t.Fatalf("close artifact: %v", err)
	}

	historyPath := filepath.Join(store.Root(), "projects_history.json")
	historyFile, err := os.OpenFile(historyPath, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0o644)
	if err != nil {
		t.Fatalf("open history: %v", err)
	}
	history := map[string]workflow.State{
		"proj-consolidated": {
			Status:          "completed",
			ConsolidatedDoc: artifactPath,
			Request: workflow.Request{
				ProjectID: "proj-consolidated",
			},
		},
	}
	if err := json.NewEncoder(historyFile).Encode(history); err != nil {
		historyFile.Close()
		t.Fatalf("write history: %v", err)
	}
	if err := historyFile.Close(); err != nil {
		t.Fatalf("close history: %v", err)
	}

	srv, err := NewServer(context.Background(), orch, nil, nil)
	if err != nil {
		t.Fatalf("new server: %v", err)
	}

	req := httptest.NewRequest(http.MethodGet, "/api/consolidated-doc?project_id=proj-consolidated", nil)
	rr := httptest.NewRecorder()
	srv.handleConsolidatedDoc(rr, req)
	if rr.Code != http.StatusOK {
		t.Fatalf("expected 200, got %d: %s", rr.Code, rr.Body.String())
	}
	if ct := rr.Header().Get("Content-Type"); ct != "application/json" {
		t.Fatalf("expected content-type application/json, got %q", ct)
	}
	var response struct {
		Artifact struct {
			Name string `json:"name"`
			Path string `json:"path"`
		} `json:"artifact"`
		Manifest workflow.ConsolidatedDoc `json:"manifest"`
	}
	if err := json.NewDecoder(rr.Body).Decode(&response); err != nil {
		t.Fatalf("decode response: %v", err)
	}
	if response.Artifact.Name != filepath.Base(artifactPath) {
		t.Fatalf("expected artifact name %q, got %q", filepath.Base(artifactPath), response.Artifact.Name)
	}
	if response.Artifact.Path != artifactPath {
		t.Fatalf("expected artifact path %q, got %q", artifactPath, response.Artifact.Path)
	}
	if response.Manifest.ProjectID != manifest.ProjectID || response.Manifest.DocumentCount != manifest.DocumentCount {
		t.Fatalf("unexpected manifest in response: %+v", response.Manifest)
	}

	downloadReq := httptest.NewRequest(http.MethodGet, "/api/consolidated-doc?project_id=proj-consolidated&download=true", nil)
	downloadRR := httptest.NewRecorder()
	srv.handleConsolidatedDoc(downloadRR, downloadReq)
	if downloadRR.Code != http.StatusOK {
		t.Fatalf("expected 200 for download, got %d: %s", downloadRR.Code, downloadRR.Body.String())
	}
	if ct := downloadRR.Header().Get("Content-Type"); ct != "application/zip" {
		t.Fatalf("expected zip content-type, got %q", ct)
	}
	if !strings.Contains(downloadRR.Header().Get("Content-Disposition"), filepath.Base(artifactPath)) {
		t.Fatalf("expected filename in disposition, got %q", downloadRR.Header().Get("Content-Disposition"))
	}
	if downloadRR.Body.Len() == 0 {
		t.Fatalf("expected download body content")
	}

	missingReq := httptest.NewRequest(http.MethodGet, "/api/consolidated-doc?project_id=unknown", nil)
	missingRR := httptest.NewRecorder()
	srv.handleConsolidatedDoc(missingRR, missingReq)
	if missingRR.Code != http.StatusNotFound {
		t.Fatalf("expected 404 for missing consolidated doc, got %d", missingRR.Code)
	}
}

func TestWorkflowDocumentationEndpoint(t *testing.T) {
	orch, store := newTestOrchestrator(t, nil, nil)
	artifactRoot := filepath.Join(store.Root(), "artifacts")
	if err := os.MkdirAll(artifactRoot, 0o755); err != nil {
		t.Fatalf("mkdir artifacts: %v", err)
	}

	docPath := filepath.Join(artifactRoot, "proj-documents.zip")
	file, err := os.Create(docPath)
	if err != nil {
		t.Fatalf("create artifact: %v", err)
	}
	writer := zip.NewWriter(file)
	entry, err := writer.Create("pgm/doc.json")
	if err != nil {
		t.Fatalf("create entry: %v", err)
	}
	payload, err := json.Marshal(kb.Doc{
		ID:      "PGM-1",
		Program: "PGM1",
		Type:    "documentation_summary",
		Summary: "PGM1 summary",
		Extra:   map[string]string{"documentation_markdown": "# Heading\nBody"},
	})
	if err != nil {
		t.Fatalf("marshal doc: %v", err)
	}
	if _, err := entry.Write(payload); err != nil {
		t.Fatalf("write doc: %v", err)
	}
	manifest, err := writer.Create("manifest.json")
	if err != nil {
		t.Fatalf("create manifest: %v", err)
	}
	if _, err := manifest.Write([]byte(`{"document_count":1}`)); err != nil {
		t.Fatalf("write manifest: %v", err)
	}
	if err := writer.Close(); err != nil {
		t.Fatalf("close archive: %v", err)
	}
	if err := file.Close(); err != nil {
		t.Fatalf("close file: %v", err)
	}

	historyPath := filepath.Join(store.Root(), "projects_history.json")
	historyFile, err := os.OpenFile(historyPath, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0o644)
	if err != nil {
		t.Fatalf("open history: %v", err)
	}
	history := map[string]workflow.State{
		"proj-docs": {
			Status: "completed",
			DocumentationArtifacts: map[string]string{
				"documentation_summary": docPath,
			},
			Request: workflow.Request{ProjectID: "proj-docs"},
		},
	}
	if err := json.NewEncoder(historyFile).Encode(history); err != nil {
		historyFile.Close()
		t.Fatalf("encode history: %v", err)
	}
	if err := historyFile.Close(); err != nil {
		t.Fatalf("close history: %v", err)
	}

	srv, err := NewServer(context.Background(), orch, nil, nil)
	if err != nil {
		t.Fatalf("new server: %v", err)
	}

	req := httptest.NewRequest(http.MethodGet, "/v1/workflow/documentation?project_id=proj-docs", nil)
	rr := httptest.NewRecorder()
	srv.handleWorkflowDocumentation(rr, req)
	if rr.Code != http.StatusOK {
		t.Fatalf("expected 200, got %d: %s", rr.Code, rr.Body.String())
	}
	var resp struct {
		Artifacts map[string][]workflow.DocumentationProgramGroup `json:"artifacts"`
	}
	if err := json.NewDecoder(rr.Body).Decode(&resp); err != nil {
		t.Fatalf("decode response: %v", err)
	}
	groups, ok := resp.Artifacts["documentation_summary"]
	if !ok {
		t.Fatalf("expected documentation_summary in response")
	}
	if len(groups) != 1 {
		t.Fatalf("expected 1 program group, got %d", len(groups))
	}
	if len(groups[0].Documents) != 1 {
		t.Fatalf("expected 1 document, got %d", len(groups[0].Documents))
	}
	if groups[0].Documents[0].Markdown != "# Heading\nBody" {
		t.Fatalf("unexpected markdown: %q", groups[0].Documents[0].Markdown)
	}
}

func TestLogsIncludeManagedServiceOutput(t *testing.T) {
	srv, _, _ := newTestServer(t, nil, nil, nil, nil)

	stdoutLine := fmt.Sprintf("stdout-%d", time.Now().UnixNano())
	stderrLine := fmt.Sprintf("stderr-%d", time.Now().UnixNano())
	command := fmt.Sprintf("echo '%s'; >&2 echo '%s'", stdoutLine, stderrLine)

	svc, err := process.Start(context.Background(), process.ServiceConfig{
		Name:    "log-writer",
		Command: "/bin/sh",
		Args:    []string{"-c", command},
	})
	if err != nil {
		t.Fatalf("start service: %v", err)
	}
	t.Cleanup(func() {
		_ = svc.Stop(context.Background())
	})

	time.Sleep(100 * time.Millisecond)
	deadline := time.Now().Add(5 * time.Second)
	component := "service/log-writer"
	foundStdout := false
	foundStderr := false
	for time.Now().Before(deadline) {
		req := httptest.NewRequest(http.MethodGet, "/v1/logs", nil)
		rr := httptest.NewRecorder()
		srv.handleLogs(rr, req)
		if rr.Code != http.StatusOK {
			t.Fatalf("unexpected status: %d", rr.Code)
		}
		var resp struct {
			Entries []struct {
				Message   string `json:"message"`
				Component string `json:"component"`
			} `json:"entries"`
		}
		if err := json.NewDecoder(rr.Body).Decode(&resp); err != nil {
			t.Fatalf("decode logs: %v", err)
		}
		for _, entry := range resp.Entries {
			if entry.Message == stdoutLine && entry.Component == component {
				foundStdout = true
			}
			if entry.Message == stderrLine && entry.Component == component {
				foundStderr = true
			}
		}
		if foundStdout && foundStderr {
			break
		}
		time.Sleep(50 * time.Millisecond)
	}
	if !foundStdout {
		t.Fatalf("stdout line not found in logs")
	}
	if !foundStderr {
		t.Fatalf("stderr line not found in logs")
	}
}
