// File path: internal/vector/chromadb_test.go
package vector

import (
	"context"
	"encoding/json"
	"errors"
	"net/http"
	"net/http/httptest"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/nicodishanthj/Katral_phase1/internal/kb"
)

type fakeChroma struct {
	t *testing.T

	mu                  sync.Mutex
	collectionName      string
	collectionID        string
	heartbeatFailures   int
	heartbeatCalls      int
	findCollectionErr   error
	createCollectionErr error
	addCalls            int
	upsertCalls         int
	queryCalls          int

	lastAddPayload    map[string]interface{}
	lastUpsertPayload map[string]interface{}

	heartbeatCalled chan struct{}
}

func newFakeChroma(t *testing.T) *fakeChroma {
	t.Helper()
	return &fakeChroma{
		t:               t,
		collectionName:  "cwa_docs",
		collectionID:    "col-123",
		heartbeatCalled: make(chan struct{}, 10),
	}
}

func (f *fakeChroma) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	switch {
	case r.URL.Path == "/api/v1/heartbeat":
		f.handleHeartbeat(w)
	case r.URL.Path == "/api/v1/collections":
		f.handleCollections(w, r)
	case strings.HasPrefix(r.URL.Path, "/api/v1/collections/") && strings.HasSuffix(r.URL.Path, "/upsert"):
		f.handleUpsert(w, r)
	case strings.HasPrefix(r.URL.Path, "/api/v1/collections/") && strings.HasSuffix(r.URL.Path, "/add"):
		f.handleAdd(w, r)
	case strings.HasPrefix(r.URL.Path, "/api/v1/collections/") && strings.HasSuffix(r.URL.Path, "/query"):
		f.handleQuery(w)
	default:
		http.NotFound(w, r)
	}
}

func (f *fakeChroma) handleHeartbeat(w http.ResponseWriter) {
	f.mu.Lock()
	f.heartbeatCalls++
	shouldFail := f.heartbeatFailures > 0
	if shouldFail {
		f.heartbeatFailures--
	}
	f.mu.Unlock()
	select {
	case f.heartbeatCalled <- struct{}{}:
	default:
	}
	if shouldFail {
		w.WriteHeader(http.StatusInternalServerError)
		_, _ = w.Write([]byte("heartbeat failure"))
		return
	}
	w.WriteHeader(http.StatusOK)
	_, _ = w.Write([]byte("ok"))
}

func (f *fakeChroma) handleCollections(w http.ResponseWriter, r *http.Request) {
	if r.Method == http.MethodGet {
		f.mu.Lock()
		err := f.findCollectionErr
		name := r.URL.Query().Get("name")
		collectionName := f.collectionName
		collectionID := f.collectionID
		f.mu.Unlock()
		if err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			_, _ = w.Write([]byte(err.Error()))
			return
		}
		resp := map[string]interface{}{"collections": []map[string]string{}}
		if collectionID != "" && (name == "" || strings.EqualFold(name, collectionName)) {
			resp["collections"] = []map[string]string{{"id": collectionID, "name": collectionName}}
		}
		w.Header().Set("Content-Type", "application/json")
		_ = json.NewEncoder(w).Encode(resp)
		return
	}
	if r.Method == http.MethodPost {
		f.mu.Lock()
		err := f.createCollectionErr
		if err == nil && f.collectionID == "" {
			f.collectionID = "generated"
		}
		id := f.collectionID
		f.mu.Unlock()
		if err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			_, _ = w.Write([]byte(err.Error()))
			return
		}
		w.Header().Set("Content-Type", "application/json")
		_ = json.NewEncoder(w).Encode(map[string]string{"id": id})
		return
	}
	w.WriteHeader(http.StatusMethodNotAllowed)
}

func (f *fakeChroma) handleAdd(w http.ResponseWriter, r *http.Request) {
	defer r.Body.Close()
	var payload map[string]interface{}
	if err := json.NewDecoder(r.Body).Decode(&payload); err != nil {
		w.WriteHeader(http.StatusBadRequest)
		_, _ = w.Write([]byte("invalid payload"))
		return
	}
	f.mu.Lock()
	f.addCalls++
	f.lastAddPayload = payload
	f.mu.Unlock()
	w.WriteHeader(http.StatusOK)
	_, _ = w.Write([]byte("added"))
}

func (f *fakeChroma) handleUpsert(w http.ResponseWriter, r *http.Request) {
	defer r.Body.Close()
	var payload map[string]interface{}
	if err := json.NewDecoder(r.Body).Decode(&payload); err != nil {
		w.WriteHeader(http.StatusBadRequest)
		_, _ = w.Write([]byte("invalid payload"))
		return
	}
	f.mu.Lock()
	f.upsertCalls++
	f.lastUpsertPayload = payload
	f.mu.Unlock()
	w.WriteHeader(http.StatusOK)
	_, _ = w.Write([]byte("upserted"))
}

func (f *fakeChroma) handleQuery(w http.ResponseWriter) {
	f.mu.Lock()
	f.queryCalls++
	f.mu.Unlock()
	w.Header().Set("Content-Type", "application/json")
	metadatas := []map[string]interface{}{{"summary": "hello"}}
	resp := map[string]interface{}{
		"ids":       [][]string{{"doc-1"}},
		"distances": [][]float64{{0.5}},
		"metadatas": [][]map[string]interface{}{metadatas},
		"documents": [][]string{{"content"}},
	}
	_ = json.NewEncoder(w).Encode(resp)
}

func (f *fakeChroma) heartbeatCount() int {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.heartbeatCalls
}

func (f *fakeChroma) addCount() int {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.addCalls
}

func (f *fakeChroma) upsertCount() int {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.upsertCalls
}

func (f *fakeChroma) queryCount() int {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.queryCalls
}

func (f *fakeChroma) lastAdd() map[string]interface{} {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.lastAddPayload
}

func (f *fakeChroma) lastUpsert() map[string]interface{} {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.lastUpsertPayload
}

func newTestClient(server *httptest.Server, fake *fakeChroma) *Client {
	return &Client{
		httpClient: server.Client(),
		baseURL:    strings.TrimRight(server.URL, "/") + "/api/v1",
		collection: fake.collectionName,
	}
}

func TestEnsureReadyRetriesHeartbeat(t *testing.T) {
	fake := newFakeChroma(t)
	fake.heartbeatFailures = 1
	server := httptest.NewServer(fake)
	t.Cleanup(server.Close)

	client := newTestClient(server, fake)

	if err := client.ensureReady(context.Background()); err != nil {
		t.Fatalf("ensureReady returned error: %v", err)
	}
	if !client.Available() {
		t.Fatalf("client should be marked available")
	}
	if fake.heartbeatCount() < 2 {
		t.Fatalf("expected at least two heartbeat attempts, got %d", fake.heartbeatCount())
	}
}

func TestEnsureReadyContextCanceled(t *testing.T) {
	fake := newFakeChroma(t)
	fake.heartbeatFailures = 10
	server := httptest.NewServer(fake)
	t.Cleanup(server.Close)

	client := newTestClient(server, fake)
	ctx, cancel := context.WithCancel(context.Background())

	done := make(chan error, 1)
	go func() {
		done <- client.ensureReady(ctx)
	}()

	select {
	case <-fake.heartbeatCalled:
	case <-time.After(2 * time.Second):
		t.Fatal("expected heartbeat to be called")
	}
	cancel()

	select {
	case err := <-done:
		if !errors.Is(err, context.Canceled) {
			t.Fatalf("expected context canceled error, got %v", err)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("ensureReady did not return after context cancellation")
	}
	if client.Available() {
		t.Fatal("client should not be marked available after cancellation")
	}
}

func TestEnsureReadyCollectionLookupFailure(t *testing.T) {
	fake := newFakeChroma(t)
	fake.findCollectionErr = errors.New("discovery failed")
	server := httptest.NewServer(fake)
	t.Cleanup(server.Close)

	client := newTestClient(server, fake)

	err := client.ensureReady(context.Background())
	if err == nil || !strings.Contains(err.Error(), "discovery failed") {
		t.Fatalf("expected discovery error, got %v", err)
	}
	if client.Available() {
		t.Fatal("client should remain unavailable on discovery failure")
	}
}

func TestUpsertDocsFlattensMetadata(t *testing.T) {
	fake := newFakeChroma(t)
	server := httptest.NewServer(fake)
	t.Cleanup(server.Close)

	client := newTestClient(server, fake)
	client.available = true
	client.collectionID = fake.collectionID

	doc := kb.Doc{
		ID:           "doc-1",
		Content:      "content",
		Program:      "PROGRAM1",
		SourcePath:   "prog.cbl",
		Summary:      "summary text",
		Technologies: []string{"COBOL", "DB2"},
	}
	vectors := [][]float32{{0.1, 0.2, 0.3}}

	if err := client.UpsertDocs(context.Background(), []kb.Doc{doc}, vectors); err != nil {
		t.Fatalf("UpsertDocs returned error: %v", err)
	}

	payload := fake.lastUpsert()
	if payload == nil {
		t.Fatal("expected payload to be captured")
	}
	metadatas, ok := payload["metadatas"].([]interface{})
	if !ok {
		t.Fatalf("expected metadatas slice, got %T", payload["metadatas"])
	}
	if len(metadatas) != 1 {
		t.Fatalf("expected 1 metadata entry, got %d", len(metadatas))
	}
	metadata, ok := metadatas[0].(map[string]interface{})
	if !ok {
		t.Fatalf("metadata entry has unexpected type %T", metadatas[0])
	}
	if got, ok := metadata["technologies"].(string); !ok || got != "COBOL, DB2" {
		t.Fatalf("expected flattened technologies, got %v", metadata["technologies"])
	}
	for _, key := range []string{"program", "source", "summary"} {
		if _, ok := metadata[key].(string); !ok {
			t.Fatalf("expected %s to be a string, got %T", key, metadata[key])
		}
	}
	for key, value := range metadata {
		if _, ok := value.(string); !ok {
			t.Fatalf("metadata %s should be a string, got %T", key, value)
		}
	}
}

func TestUpsertDocsAllowsDuplicateIDs(t *testing.T) {
	fake := newFakeChroma(t)
	server := httptest.NewServer(fake)
	t.Cleanup(server.Close)

	client := newTestClient(server, fake)
	client.available = true
	client.collectionID = fake.collectionID

	doc := kb.Doc{ID: "doc-1", Content: "content"}
	vectors := [][]float32{{0.1, 0.2, 0.3}}

	ctx := context.Background()
	if err := client.UpsertDocs(ctx, []kb.Doc{doc}, vectors); err != nil {
		t.Fatalf("first UpsertDocs returned error: %v", err)
	}
	if err := client.UpsertDocs(ctx, []kb.Doc{doc}, vectors); err != nil {
		t.Fatalf("second UpsertDocs returned error: %v", err)
	}

	if fake.upsertCount() != 2 {
		t.Fatalf("expected 2 upsert calls, got %d", fake.upsertCount())
	}
}

func TestPublicEntryPointsTriggerRecovery(t *testing.T) {
	fake := newFakeChroma(t)
	fake.heartbeatFailures = 1
	server := httptest.NewServer(fake)
	t.Cleanup(server.Close)

	client := newTestClient(server, fake)
	client.available = false
	client.collectionID = ""

	ctx := context.Background()

	if err := client.EnsureCollection(ctx, 3); err != nil {
		t.Fatalf("EnsureCollection failed: %v", err)
	}
	if !client.Available() {
		t.Fatal("client should be available after EnsureCollection")
	}

	client.available = false
	client.collectionID = ""
	fake.heartbeatFailures = 1
	docs := []kb.Doc{{ID: "doc-1", Content: "hello"}}
	vecs := [][]float32{{0.1, 0.2}}
	if err := client.UpsertDocs(ctx, docs, vecs); err != nil {
		t.Fatalf("UpsertDocs failed: %v", err)
	}
	if fake.upsertCount() != 1 {
		t.Fatalf("expected 1 upsert call, got %d", fake.upsertCount())
	}

	client.available = false
	client.collectionID = ""
	fake.heartbeatFailures = 1
	results, err := client.Search(ctx, []float32{0.5, 0.9}, 2)
	if err != nil {
		t.Fatalf("Search failed: %v", err)
	}
	if len(results) != 1 || results[0].ID != "doc-1" {
		t.Fatalf("unexpected search results: %+v", results)
	}
	if fake.queryCount() != 1 {
		t.Fatalf("expected 1 query call, got %d", fake.queryCount())
	}
}
