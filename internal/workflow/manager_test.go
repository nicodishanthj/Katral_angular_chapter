// File path: internal/workflow/manager_test.go
package workflow

import (
	"archive/zip"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/nicodishanthj/Katral_phase1/internal/graph"
	"github.com/nicodishanthj/Katral_phase1/internal/kb"
	"github.com/nicodishanthj/Katral_phase1/internal/memory"
	"github.com/nicodishanthj/Katral_phase1/internal/retriever"
)

func TestManagerStatusReturnsPersistedState(t *testing.T) {
	t.Helper()

	tmp := t.TempDir()
	store, err := memory.NewStore(filepath.Join(tmp, "docs.jsonl"))
	if err != nil {
		t.Fatalf("new store: %v", err)
	}

	retr := retriever.New(nil, nil)
	mgr := NewManager(store, nil, retr, nil, nil, nil, graph.NoopDependencyService(), nil, nil, nil)

	now := time.Now().UTC()
	artifactPath := filepath.Join(tmp, "artifacts", "proj-1.zip")
	state := State{
		Status:         "completed",
		Running:        false,
		StartedAt:      &now,
		CompletedAt:    &now,
		SpringArtifact: artifactPath,
		Request: Request{
			ProjectID: "proj-1",
			Flow:      "test-flow",
		},
	}

	mgr.persistProjectState("proj-1", state)

	retr2 := retriever.New(nil, nil)
	mgr2 := NewManager(store, nil, retr2, nil, nil, nil, graph.NoopDependencyService(), nil, nil, nil)

	got := mgr2.Status("proj-1")
	if got.Status != "completed" {
		t.Fatalf("expected status completed, got %q", got.Status)
	}
	if got.Request.ProjectID != "proj-1" {
		t.Fatalf("expected project_id proj-1, got %q", got.Request.ProjectID)
	}
	if got.CompletedAt == nil {
		t.Fatalf("expected completed_at to be restored")
	}
	if got.SpringArtifact != artifactPath {
		t.Fatalf("expected spring artifact %q, got %q", artifactPath, got.SpringArtifact)
	}

	missing := mgr2.Status("unknown")
	if missing.Request.ProjectID != "unknown" {
		t.Fatalf("expected request project id to be populated for unknown project")
	}
	if missing.Status != "idle" {
		t.Fatalf("expected idle status for unknown project, got %q", missing.Status)
	}
}

func TestManagerSpringArtifactPathValidatesLocation(t *testing.T) {
	t.Helper()

	tmp := t.TempDir()
	store, err := memory.NewStore(filepath.Join(tmp, "docs.jsonl"))
	if err != nil {
		t.Fatalf("new store: %v", err)
	}
	retr := retriever.New(nil, nil)
	mgr := NewManager(store, nil, retr, nil, nil, nil, graph.NoopDependencyService(), nil, nil, nil)

	artifactRoot := filepath.Join(store.Root(), "artifacts")
	if err := os.MkdirAll(artifactRoot, 0o755); err != nil {
		t.Fatalf("mkdir artifact root: %v", err)
	}

	inside := filepath.Join(artifactRoot, "proj-valid.zip")
	if err := os.WriteFile(inside, []byte("artifact"), 0o644); err != nil {
		t.Fatalf("write artifact: %v", err)
	}
	validState := State{Status: "completed", SpringArtifact: inside, Request: Request{ProjectID: "proj-valid"}}
	mgr.persistProjectState("proj-valid", validState)

	resolved, err := mgr.SpringArtifactPath("proj-valid")
	if err != nil {
		t.Fatalf("SpringArtifactPath valid: %v", err)
	}
	if resolved != inside {
		t.Fatalf("expected path %q, got %q", inside, resolved)
	}

	outside := filepath.Join(tmp, "outside.zip")
	if err := os.WriteFile(outside, []byte("artifact"), 0o644); err != nil {
		t.Fatalf("write outside artifact: %v", err)
	}
	mgr.persistProjectState("proj-outside", State{Status: "completed", SpringArtifact: outside, Request: Request{ProjectID: "proj-outside"}})
	if _, err := mgr.SpringArtifactPath("proj-outside"); !errors.Is(err, ErrSpringArtifactInvalid) {
		t.Fatalf("expected invalid error, got %v", err)
	}

	missing := filepath.Join(artifactRoot, "missing.zip")
	mgr.persistProjectState("proj-missing", State{Status: "completed", SpringArtifact: missing, Request: Request{ProjectID: "proj-missing"}})
	if _, err := mgr.SpringArtifactPath("proj-missing"); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("expected missing artifact error, got %v", err)
	}

	mgr.persistProjectState("proj-none", State{Status: "completed", Request: Request{ProjectID: "proj-none"}})
	if _, err := mgr.SpringArtifactPath("proj-none"); !errors.Is(err, ErrSpringArtifactNotFound) {
		t.Fatalf("expected not found error, got %v", err)
	}
}

func TestManagerDocumentationArtifactPathValidatesLocation(t *testing.T) {
	t.Helper()

	tmp := t.TempDir()
	store, err := memory.NewStore(filepath.Join(tmp, "docs.jsonl"))
	if err != nil {
		t.Fatalf("new store: %v", err)
	}
	retr := retriever.New(nil, nil)
	mgr := NewManager(store, nil, retr, nil, nil, nil, graph.NoopDependencyService(), nil, nil, nil)

	artifactRoot := filepath.Join(store.Root(), "artifacts")
	if err := os.MkdirAll(artifactRoot, 0o755); err != nil {
		t.Fatalf("mkdir artifact root: %v", err)
	}

	summaryPath := filepath.Join(artifactRoot, "proj-doc-summary.zip")
	if err := os.WriteFile(summaryPath, []byte("doc"), 0o644); err != nil {
		t.Fatalf("write doc artifact: %v", err)
	}
	state := State{
		Status:                 "completed",
		DocumentationArtifacts: map[string]string{"documentation_summary": summaryPath},
		Request:                Request{ProjectID: "proj-doc"},
	}
	mgr.persistProjectState("proj-doc", state)

	resolved, err := mgr.DocumentationArtifactPath("proj-doc", "documentation_summary")
	if err != nil {
		t.Fatalf("DocumentationArtifactPath valid: %v", err)
	}
	if resolved != summaryPath {
		t.Fatalf("expected summary path %q, got %q", summaryPath, resolved)
	}

	outside := filepath.Join(tmp, "proj-doc-outside.zip")
	if err := os.WriteFile(outside, []byte("doc"), 0o644); err != nil {
		t.Fatalf("write outside doc artifact: %v", err)
	}
	mgr.persistProjectState("proj-doc-outside", State{
		Status:                 "completed",
		DocumentationArtifacts: map[string]string{"documentation_summary": outside},
		Request:                Request{ProjectID: "proj-doc-outside"},
	})
	if _, err := mgr.DocumentationArtifactPath("proj-doc-outside", "documentation_summary"); !errors.Is(err, ErrArtifactInvalid) {
		t.Fatalf("expected invalid doc artifact error, got %v", err)
	}

	mgr.persistProjectState("proj-doc-missing", State{
		Status:                 "completed",
		DocumentationArtifacts: map[string]string{"documentation_summary": filepath.Join(artifactRoot, "missing.zip")},
		Request:                Request{ProjectID: "proj-doc-missing"},
	})
	if _, err := mgr.DocumentationArtifactPath("proj-doc-missing", "documentation_summary"); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("expected missing doc artifact error, got %v", err)
	}

	mgr.persistProjectState("proj-doc-none", State{Status: "completed", Request: Request{ProjectID: "proj-doc-none"}})
	if _, err := mgr.DocumentationArtifactPath("proj-doc-none", "documentation_summary"); !errors.Is(err, ErrArtifactNotFound) {
		t.Fatalf("expected doc artifact not found error, got %v", err)
	}
}

func TestManagerLoadDocumentationArtifacts(t *testing.T) {
	t.Helper()

	tmp := t.TempDir()
	store, err := memory.NewStore(filepath.Join(tmp, "docs.jsonl"))
	if err != nil {
		t.Fatalf("new store: %v", err)
	}
	retr := retriever.New(nil, nil)
	mgr := NewManager(store, nil, retr, nil, nil, nil, graph.NoopDependencyService(), nil, nil, nil)

	artifactRoot := filepath.Join(store.Root(), "artifacts")
	if err := os.MkdirAll(artifactRoot, 0o755); err != nil {
		t.Fatalf("mkdir artifact root: %v", err)
	}

	summaryPath := filepath.Join(artifactRoot, "proj-docs-summary.zip")
	crossPath := filepath.Join(artifactRoot, "proj-docs-cross.zip")

	writeArchive := func(path string, docs []kb.Doc) {
		t.Helper()
		file, err := os.Create(path)
		if err != nil {
			t.Fatalf("create archive: %v", err)
		}
		zipWriter := zip.NewWriter(file)
		for idx, doc := range docs {
			program := strings.TrimSpace(doc.Program)
			if program == "" {
				program = "project"
			}
			entryName := fmt.Sprintf("%s/doc-%d.json", strings.ToLower(program), idx+1)
			writer, err := zipWriter.Create(entryName)
			if err != nil {
				t.Fatalf("create entry: %v", err)
			}
			payload, err := json.Marshal(doc)
			if err != nil {
				t.Fatalf("marshal doc: %v", err)
			}
			if _, err := writer.Write(payload); err != nil {
				t.Fatalf("write doc: %v", err)
			}
		}
		manifestWriter, err := zipWriter.Create("manifest.json")
		if err != nil {
			t.Fatalf("create manifest: %v", err)
		}
		manifest := map[string]interface{}{"document_count": len(docs)}
		manifestPayload, err := json.Marshal(manifest)
		if err != nil {
			t.Fatalf("marshal manifest: %v", err)
		}
		if _, err := manifestWriter.Write(manifestPayload); err != nil {
			t.Fatalf("write manifest: %v", err)
		}
		if err := zipWriter.Close(); err != nil {
			t.Fatalf("close archive: %v", err)
		}
		if err := file.Close(); err != nil {
			t.Fatalf("close file: %v", err)
		}
	}

	writeArchive(summaryPath, []kb.Doc{
		{
			ID:         "PGM1-summary",
			Program:    "PGM1",
			Summary:    "Overview for PGM1",
			Type:       "documentation_summary",
			Extra:      map[string]string{"documentation_markdown": "# PGM1\nDetails"},
			SourcePath: "pgm1.cbl",
		},
		{
			ID:      "project-summary",
			Program: "",
			Summary: "Portfolio overview",
			Type:    "documentation_summary",
		},
	})

	writeArchive(crossPath, []kb.Doc{
		{
			ID:      "PGM2-cross",
			Program: "PGM2",
			Type:    "documentation_cross_reference",
			Content: "Cross reference content",
		},
		{
			ID:      "PGM3-empty",
			Program: "PGM3",
			Type:    "documentation_cross_reference",
		},
	})

	mgr.persistProjectState("proj-docs", State{
		Status: "completed",
		DocumentationArtifacts: map[string]string{
			"documentation_summary":         summaryPath,
			"documentation_cross_reference": crossPath,
		},
		Request: Request{ProjectID: "proj-docs"},
	})

	rendered, err := mgr.LoadDocumentationArtifacts("proj-docs")
	if err != nil {
		t.Fatalf("LoadDocumentationArtifacts: %v", err)
	}
	if len(rendered) != 2 {
		t.Fatalf("expected 2 artifact types, got %d", len(rendered))
	}

	summaryGroups, ok := rendered["documentation_summary"]
	if !ok {
		t.Fatalf("expected documentation_summary group")
	}
	if len(summaryGroups) != 2 {
		t.Fatalf("expected 2 summary program groups, got %d", len(summaryGroups))
	}
	summaryLookup := make(map[string]DocumentationProgramGroup, len(summaryGroups))
	for _, group := range summaryGroups {
		summaryLookup[group.Program] = group
	}
	pgm1, ok := summaryLookup["PGM1"]
	if !ok {
		t.Fatalf("expected PGM1 summary group")
	}
	if len(pgm1.Documents) != 1 {
		t.Fatalf("expected 1 document for PGM1 summary, got %d", len(pgm1.Documents))
	}
	if pgm1.Documents[0].Markdown != "# PGM1\nDetails" {
		t.Fatalf("unexpected markdown for PGM1 summary: %q", pgm1.Documents[0].Markdown)
	}
	if pgm1.Documents[0].Source != "pgm1.cbl" {
		t.Fatalf("expected source path propagated, got %q", pgm1.Documents[0].Source)
	}

	projectGroup, ok := summaryLookup["Project"]
	if !ok {
		t.Fatalf("expected Project summary group")
	}
	if len(projectGroup.Documents) != 1 {
		t.Fatalf("expected 1 document for Project summary, got %d", len(projectGroup.Documents))
	}
	if projectGroup.Documents[0].Markdown != "Portfolio overview" {
		t.Fatalf("expected summary fallback markdown, got %q", projectGroup.Documents[0].Markdown)
	}

	crossGroups, ok := rendered["documentation_cross_reference"]
	if !ok {
		t.Fatalf("expected documentation_cross_reference group")
	}
	if len(crossGroups) != 1 {
		t.Fatalf("expected 1 cross-reference group, got %d", len(crossGroups))
	}
	crossDocs := crossGroups[0].Documents
	if len(crossDocs) != 1 {
		t.Fatalf("expected 1 cross-reference document, got %d", len(crossDocs))
	}
	if crossDocs[0].Markdown != "Cross reference content" {
		t.Fatalf("unexpected cross-reference markdown: %q", crossDocs[0].Markdown)
	}
}

func TestManagerConversionArtifactPathValidatesLocation(t *testing.T) {
	t.Helper()

	tmp := t.TempDir()
	store, err := memory.NewStore(filepath.Join(tmp, "docs.jsonl"))
	if err != nil {
		t.Fatalf("new store: %v", err)
	}
	retr := retriever.New(nil, nil)
	mgr := NewManager(store, nil, retr, nil, nil, nil, graph.NoopDependencyService(), nil, nil, nil)

	artifactRoot := filepath.Join(store.Root(), "artifacts")
	if err := os.MkdirAll(artifactRoot, 0o755); err != nil {
		t.Fatalf("mkdir artifact root: %v", err)
	}

	summaryPath := filepath.Join(artifactRoot, "proj-conv-summary.zip")
	if err := os.WriteFile(summaryPath, []byte("conv"), 0o644); err != nil {
		t.Fatalf("write conversion artifact: %v", err)
	}
	state := State{
		Status:              "completed",
		ConversionArtifacts: map[string]string{"conversion_summary": summaryPath},
		Request:             Request{ProjectID: "proj-conv"},
	}
	mgr.persistProjectState("proj-conv", state)

	resolved, err := mgr.ConversionArtifactPath("proj-conv", "conversion_summary")
	if err != nil {
		t.Fatalf("ConversionArtifactPath valid: %v", err)
	}
	if resolved != summaryPath {
		t.Fatalf("expected conversion summary path %q, got %q", summaryPath, resolved)
	}

	outside := filepath.Join(tmp, "proj-conv-outside.zip")
	if err := os.WriteFile(outside, []byte("conv"), 0o644); err != nil {
		t.Fatalf("write outside conversion artifact: %v", err)
	}
	mgr.persistProjectState("proj-conv-outside", State{
		Status:              "completed",
		ConversionArtifacts: map[string]string{"conversion_summary": outside},
		Request:             Request{ProjectID: "proj-conv-outside"},
	})
	if _, err := mgr.ConversionArtifactPath("proj-conv-outside", "conversion_summary"); !errors.Is(err, ErrArtifactInvalid) {
		t.Fatalf("expected invalid conversion artifact error, got %v", err)
	}

	mgr.persistProjectState("proj-conv-missing", State{
		Status:              "completed",
		ConversionArtifacts: map[string]string{"conversion_summary": filepath.Join(artifactRoot, "missing.zip")},
		Request:             Request{ProjectID: "proj-conv-missing"},
	})
	if _, err := mgr.ConversionArtifactPath("proj-conv-missing", "conversion_summary"); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("expected missing conversion artifact error, got %v", err)
	}

	mgr.persistProjectState("proj-conv-none", State{Status: "completed", Request: Request{ProjectID: "proj-conv-none"}})
	if _, err := mgr.ConversionArtifactPath("proj-conv-none", "conversion_summary"); !errors.Is(err, ErrArtifactNotFound) {
		t.Fatalf("expected conversion artifact not found error, got %v", err)
	}
}
