// File path: internal/data/orchestrator/orchestrator_test.go
package orchestrator

import (
	"context"
	"path/filepath"
	"testing"
	"time"

	"github.com/nicodishanthj/Katral_phase1/internal/graph"
	"github.com/nicodishanthj/Katral_phase1/internal/kb"
	"github.com/nicodishanthj/Katral_phase1/internal/vector"
)

func TestLoadConfigDefaults(t *testing.T) {
	t.Setenv("KATRAL_MEMORY_PATH", "")
	t.Setenv("KATRAL_CATALOG_PATH", "")
	t.Setenv("KATRAL_SYNC_INTERVAL", "")
	t.Setenv("KATRAL_SYNC_TIMEOUT", "")
	t.Setenv("KATRAL_SYNC_RETRIES", "")
	t.Setenv("KATRAL_SYNC_BACKOFF", "")

	cfg, err := LoadConfig()
	if err != nil {
		t.Fatalf("LoadConfig: %v", err)
	}
	defaults := DefaultConfig()
	if cfg != defaults {
		t.Fatalf("LoadConfig defaults mismatch: %#v", cfg)
	}
}

func TestLoadConfigEnvironmentOverrides(t *testing.T) {
	t.Setenv("KATRAL_MEMORY_PATH", "/tmp/docs.jsonl")
	t.Setenv("KATRAL_CATALOG_PATH", "/tmp/catalog.db")
	t.Setenv("KATRAL_SYNC_INTERVAL", "45s")
	t.Setenv("KATRAL_SYNC_TIMEOUT", "5s")
	t.Setenv("KATRAL_SYNC_RETRIES", "7")
	t.Setenv("KATRAL_SYNC_BACKOFF", "150ms")

	cfg, err := LoadConfig()
	if err != nil {
		t.Fatalf("LoadConfig: %v", err)
	}
	if cfg.MemoryPath != "/tmp/docs.jsonl" {
		t.Errorf("MemoryPath = %q", cfg.MemoryPath)
	}
	if cfg.SQLitePath != "/tmp/catalog.db" {
		t.Errorf("SQLitePath = %q", cfg.SQLitePath)
	}
	if cfg.SyncInterval != 45*time.Second {
		t.Errorf("SyncInterval = %v", cfg.SyncInterval)
	}
	if cfg.SyncTimeout != 5*time.Second {
		t.Errorf("SyncTimeout = %v", cfg.SyncTimeout)
	}
	if cfg.MaxSyncRetries != 7 {
		t.Errorf("MaxSyncRetries = %d", cfg.MaxSyncRetries)
	}
	if cfg.RetryBackoff != 150*time.Millisecond {
		t.Errorf("RetryBackoff = %v", cfg.RetryBackoff)
	}
}

func TestNewInitializesStores(t *testing.T) {
	clearVectorEnv(t)
	clearGraphEnv(t)

	dir := t.TempDir()
	cfg := Config{
		MemoryPath: filepath.Join(dir, "docs.jsonl"),
		SQLitePath: filepath.Join(dir, "catalog.db"),
	}
	orch, err := New(context.Background(), cfg, WithSyncDisabled())
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	t.Cleanup(func() { _ = orch.Close() })

	if orch.Memory() == nil {
		t.Fatal("Memory store not initialised")
	}
	if orch.Metadata() == nil {
		t.Fatal("Metadata store not initialised")
	}
	if orch.Catalog() == nil {
		t.Fatal("Catalog store not initialised")
	}
	if orch.Vector() != nil {
		t.Fatalf("Vector store should not be configured")
	}
	if orch.Graph() != nil {
		t.Fatalf("Graph client should not be configured")
	}
}

func TestNewWithOptionalComponents(t *testing.T) {
	dir := t.TempDir()
	cfg := Config{
		MemoryPath: filepath.Join(dir, "docs.jsonl"),
		SQLitePath: filepath.Join(dir, "catalog.db"),
	}
	vec := &stubVector{}
	graphClient := &stubGraph{}
	orch, err := New(context.Background(), cfg, WithSyncDisabled(), WithVectorStore(vec), WithGraphClient(graphClient))
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	if orch.Vector() != vec {
		t.Fatalf("Vector store not applied")
	}
	if orch.Graph() != graphClient {
		t.Fatalf("Graph client not applied")
	}
	if err := orch.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	if graphClient.closed != 1 {
		t.Fatalf("expected graph close count 1, got %d", graphClient.closed)
	}
}

func clearVectorEnv(t *testing.T) {
	t.Helper()
	keys := []string{
		"CHROMADB_CONFIG_FILE",
		"CHROMADB_HOST",
		"CHROMADB_PORT",
		"CHROMADB_SCHEME",
		"CHROMADB_COLLECTION",
		"CHROMADB_API_KEY",
		"CHROMADB_TIMEOUT",
		"CHROMADB_HTTP_MAX_IDLE_CONNS",
		"CHROMADB_HTTP_MAX_IDLE_PER_HOST",
		"CHROMADB_HTTP_MAX_CONNS_PER_HOST",
		"CHROMADB_HTTP_IDLE_CONN_TIMEOUT",
	}
	for _, key := range keys {
		t.Setenv(key, "")
	}
}

func clearGraphEnv(t *testing.T) {
	t.Helper()
	keys := []string{
		"KUZU_CONFIG_FILE",
		"KUZU_ENDPOINT",
		"KUZU_DATABASE",
		"KUZU_USERNAME",
		"KUZU_PASSWORD",
		"KUZU_TIMEOUT",
		"KUZU_MAX_CONNECTIONS",
		"KUZU_HTTP_MAX_IDLE_CONNS",
		"KUZU_HTTP_MAX_IDLE_PER_HOST",
		"KUZU_HTTP_MAX_CONNS_PER_HOST",
		"KUZU_HTTP_IDLE_CONN_TIMEOUT",
	}
	for _, key := range keys {
		t.Setenv(key, "")
	}
}

type stubVector struct{}

func (s *stubVector) Available() bool                                         { return true }
func (s *stubVector) SetCollection(string)                                    {}
func (s *stubVector) Collection() string                                      { return "stub" }
func (s *stubVector) EnsureCollection(context.Context, int) error             { return nil }
func (s *stubVector) UpsertDocs(context.Context, []kb.Doc, [][]float32) error { return nil }
func (s *stubVector) Search(context.Context, []float32, int) ([]vector.SearchResult, error) {
	return nil, nil
}

type stubGraph struct {
	closed int
}

func (s *stubGraph) Available() bool                                        { return true }
func (s *stubGraph) EnsureSchema(context.Context) error                     { return nil }
func (s *stubGraph) InsertProgram(context.Context, graph.Program) error     { return nil }
func (s *stubGraph) InsertCall(context.Context, graph.Call) error           { return nil }
func (s *stubGraph) InsertParagraph(context.Context, graph.Paragraph) error { return nil }
func (s *stubGraph) InsertDataFlow(context.Context, graph.DataFlow) error   { return nil }
func (s *stubGraph) Close() error {
	s.closed++
	return nil
}
