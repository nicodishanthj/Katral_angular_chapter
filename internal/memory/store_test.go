// File path: internal/memory/store_test.go
package memory

import (
	"context"
	"path/filepath"
	"strings"
	"testing"

	"github.com/nicodishanthj/Katral_phase1/internal/kb"
)

func TestReplaceDocsOverwritesExistingContent(t *testing.T) {
	dir := t.TempDir()
	store, err := NewStore(filepath.Join(dir, "docs.jsonl"))
	if err != nil {
		t.Fatalf("new store: %v", err)
	}
	ctx := context.Background()
	initial := []kb.Doc{{ID: "1", Content: "initial"}}
	if err := store.AppendDocs(ctx, "proj-1", initial); err != nil {
		t.Fatalf("append docs: %v", err)
	}
	replacement := []kb.Doc{{ID: "2", Content: "replacement"}}
	if err := store.ReplaceDocs(ctx, "proj-1", replacement); err != nil {
		t.Fatalf("replace docs: %v", err)
	}
	docs, err := store.AllDocs(ctx, "proj-1")
	if err != nil {
		t.Fatalf("all docs: %v", err)
	}
	if len(docs) != 1 {
		t.Fatalf("expected 1 doc, got %d", len(docs))
	}
	if docs[0].ID != "2" || docs[0].Content != "replacement" {
		t.Fatalf("unexpected doc: %+v", docs[0])
	}
}

func TestReplaceDocsClearsStoreWhenEmpty(t *testing.T) {
	dir := t.TempDir()
	store, err := NewStore(filepath.Join(dir, "docs.jsonl"))
	if err != nil {
		t.Fatalf("new store: %v", err)
	}
	ctx := context.Background()
	if err := store.AppendDocs(ctx, "proj-2", []kb.Doc{{ID: "1", Content: "initial"}}); err != nil {
		t.Fatalf("append docs: %v", err)
	}
	if err := store.ReplaceDocs(ctx, "proj-2", nil); err != nil {
		t.Fatalf("replace docs: %v", err)
	}
	docs, err := store.AllDocs(ctx, "proj-2")
	if err != nil {
		t.Fatalf("all docs: %v", err)
	}
	if len(docs) != 0 {
		t.Fatalf("expected store to be empty, got %d docs", len(docs))
	}
}

func TestAllDocsHandlesLargeDocuments(t *testing.T) {
	dir := t.TempDir()
	store, err := NewStore(filepath.Join(dir, "docs.jsonl"))
	if err != nil {
		t.Fatalf("new store: %v", err)
	}
	ctx := context.Background()
	largeContent := strings.Repeat("large document content ", 1<<15)
	doc := kb.Doc{ID: "large", Content: largeContent}
	if len(doc.Content) <= 64<<10 {
		t.Fatalf("doc content too small for test: %d bytes", len(doc.Content))
	}
	if err := store.AppendDocs(ctx, "proj-large", []kb.Doc{doc}); err != nil {
		t.Fatalf("append docs: %v", err)
	}
	docs, err := store.AllDocs(ctx, "proj-large")
	if err != nil {
		t.Fatalf("all docs: %v", err)
	}
	if len(docs) != 1 {
		t.Fatalf("expected 1 doc, got %d", len(docs))
	}
	if docs[0].ID != doc.ID || docs[0].Content != doc.Content {
		t.Fatalf("unexpected doc mismatch")
	}
}

func TestProjectsListsStoredContexts(t *testing.T) {
	dir := t.TempDir()
	store, err := NewStore(filepath.Join(dir, "docs.jsonl"))
	if err != nil {
		t.Fatalf("new store: %v", err)
	}
	ctx := context.Background()
	if err := store.AppendDocs(ctx, "proj-a", []kb.Doc{{ID: "1"}}); err != nil {
		t.Fatalf("append proj-a: %v", err)
	}
	if err := store.AppendDocs(ctx, "proj-b", []kb.Doc{{ID: "2"}, {ID: "3"}}); err != nil {
		t.Fatalf("append proj-b: %v", err)
	}
	infos, err := store.Projects(ctx)
	if err != nil {
		t.Fatalf("projects: %v", err)
	}
	if len(infos) != 2 {
		t.Fatalf("expected 2 projects, got %d", len(infos))
	}
	got := map[string]int{}
	for _, info := range infos {
		got[info.ID] = info.Documents
	}
	if got["proj-a"] != 1 || got["proj-b"] != 2 {
		t.Fatalf("unexpected project info: %#v", got)
	}
}
