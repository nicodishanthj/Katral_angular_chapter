// File path: internal/workflow/runner_test.go
package workflow

import (
	"context"
	"encoding/base64"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/nicodishanthj/Katral_phase1/internal/graph"
	"github.com/nicodishanthj/Katral_phase1/internal/kb"
	"github.com/nicodishanthj/Katral_phase1/internal/memory"
	"github.com/nicodishanthj/Katral_phase1/internal/retriever"
)

func TestPackageSpringProjectCreatesArchiveAndLogs(t *testing.T) {
	t.Helper()

	tmp := t.TempDir()
	store, err := memory.NewStore(filepath.Join(tmp, "docs.jsonl"))
	if err != nil {
		t.Fatalf("new store: %v", err)
	}
	retr := retriever.New(nil, nil)
	mgr := NewManager(store, nil, retr, nil, nil, nil, graph.NoopDependencyService(), nil, nil, nil)

	springDir := filepath.Join(tmp, "spring")
	if err := os.MkdirAll(filepath.Join(springDir, "src", "main"), 0o755); err != nil {
		t.Fatalf("mkdir spring dir: %v", err)
	}
	if err := os.WriteFile(filepath.Join(springDir, "pom.xml"), []byte("<project/>"), 0o644); err != nil {
		t.Fatalf("write pom: %v", err)
	}
	if err := os.WriteFile(filepath.Join(springDir, "src", "main", "App.java"), []byte("class App {}"), 0o644); err != nil {
		t.Fatalf("write source: %v", err)
	}

	artifactPath, err := mgr.packageSpringProject(context.Background(), "Proj Example", springDir)
	if err != nil {
		t.Fatalf("package spring project: %v", err)
	}
	info, err := os.Stat(artifactPath)
	if err != nil {
		t.Fatalf("stat artifact: %v", err)
	}
	if info.Size() == 0 {
		t.Fatalf("expected artifact to have content")
	}
	base := filepath.Base(artifactPath)
	if !strings.HasPrefix(base, "proj-example-") || !strings.HasSuffix(base, ".json") {
		t.Fatalf("unexpected artifact name %q", base)
	}

	data, err := os.ReadFile(artifactPath)
	if err != nil {
		t.Fatalf("read artifact: %v", err)
	}
	var payload struct {
		ProjectID string `json:"project_id"`
		Root      string `json:"root"`
		Files     []struct {
			Path    string `json:"path"`
			Content string `json:"content"`
		} `json:"files"`
	}
	if err := json.Unmarshal(data, &payload); err != nil {
		t.Fatalf("unmarshal artifact: %v", err)
	}
	if !strings.EqualFold(payload.ProjectID, "proj example") {
		t.Fatalf("unexpected project id in artifact: %+v", payload)
	}
	if payload.Root != "spring" {
		t.Fatalf("expected root to be spring, got %q", payload.Root)
	}
	files := make(map[string]string)
	for _, file := range payload.Files {
		files[file.Path] = file.Content
	}
	pom, ok := files["pom.xml"]
	if !ok {
		t.Fatalf("expected pom.xml in artifact, got %v", files)
	}
	pomBytes, err := base64.StdEncoding.DecodeString(pom)
	if err != nil {
		t.Fatalf("decode pom content: %v", err)
	}
	if string(pomBytes) != "<project/>" {
		t.Fatalf("unexpected pom content: %s", string(pomBytes))
	}
	app, ok := files["src/main/App.java"]
	if !ok {
		t.Fatalf("expected App.java in artifact, got %v", files)
	}
	appBytes, err := base64.StdEncoding.DecodeString(app)
	if err != nil {
		t.Fatalf("decode app content: %v", err)
	}
	if string(appBytes) != "class App {}" {
		t.Fatalf("unexpected app content: %s", string(appBytes))
	}

	logs := mgr.Logs()
	foundLog := false
	for _, entry := range logs {
		if strings.Contains(entry.Message, "Packaged Spring project for project Proj Example") {
			foundLog = true
			break
		}
	}
	if !foundLog {
		t.Fatalf("expected packaging log entry, got %#v", logs)
	}
}

func TestPackageConversionDocumentsCreatesArchives(t *testing.T) {
	t.Helper()

	tmp := t.TempDir()
	store, err := memory.NewStore(filepath.Join(tmp, "docs.jsonl"))
	if err != nil {
		t.Fatalf("new store: %v", err)
	}
	retr := retriever.New(nil, nil)
	mgr := NewManager(store, nil, retr, nil, nil, nil, graph.NoopDependencyService(), nil, nil, nil)

	docs := []kb.Doc{
		{ID: "PGM1:conv-summary", Program: "PGM1", Type: "conversion_summary", Content: "Summary"},
		{ID: "PGM1:conv-mapping", Program: "PGM1", Type: "conversion_mapping", Content: "Mapping"},
		{ID: "PGM1:conv-source", Program: "PGM1", Type: "conversion_source", Content: "Source"},
	}

	artifacts, err := mgr.packageConversion(context.Background(), "Proj Conv", docs)
	if err != nil {
		t.Fatalf("package conversion docs: %v", err)
	}
	if len(artifacts) != 3 {
		t.Fatalf("expected three conversion artifacts, got %d", len(artifacts))
	}
	for kind, path := range artifacts {
		if strings.TrimSpace(path) == "" {
			t.Fatalf("empty artifact path for %s", kind)
		}
		info, err := os.Stat(path)
		if err != nil {
			t.Fatalf("stat artifact %s: %v", kind, err)
		}
		if info.Size() == 0 {
			t.Fatalf("expected artifact %s to have content", kind)
		}
		if filepath.Ext(path) != ".json" {
			t.Fatalf("expected artifact %s to be json, got %s", kind, path)
		}
		data, err := os.ReadFile(path)
		if err != nil {
			t.Fatalf("read %s artifact: %v", kind, err)
		}
		var payload struct {
			ArtifactType  string   `json:"artifact_type"`
			DocumentCount int      `json:"document_count"`
			DocumentIDs   []string `json:"document_ids"`
			Documents     []kb.Doc `json:"documents"`
		}
		if err := json.Unmarshal(data, &payload); err != nil {
			t.Fatalf("decode %s artifact: %v", kind, err)
		}
		if payload.ArtifactType != kind {
			t.Fatalf("expected artifact type %s, got %s", kind, payload.ArtifactType)
		}
		if payload.DocumentCount != len(payload.Documents) {
			t.Fatalf("document count mismatch: %+v", payload)
		}
		if len(payload.DocumentIDs) == 0 {
			t.Fatalf("expected document ids for %s", kind)
		}
	}
}
