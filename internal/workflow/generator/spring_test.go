// File path: internal/workflow/generator/spring_test.go
package generator

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"

	ctxbuilder "github.com/nicodishanthj/Katral_phase1/internal/context"
	"github.com/nicodishanthj/Katral_phase1/internal/graph"
	"github.com/nicodishanthj/Katral_phase1/internal/kb"
	"github.com/nicodishanthj/Katral_phase1/internal/metadata"
)

func TestSpringGeneratorProducesServiceSkeleton(t *testing.T) {
	t.Helper()
	ctx := context.Background()
	meta := &fakeMetadataStore{programs: []metadata.ProgramRecord{{
		ProjectID:  "proj-123",
		Name:       "ACCTUPD",
		SourcePath: "ACCTUPD.cbl",
		Summary:    "Handles account updates",
		Technologies: []string{
			"COBOL", "CICS",
		},
	}, {
		ProjectID: "proj-123",
		Name:      "BALANCE",
		Summary:   "Retrieves balance",
	}}}
	builder := &fakeBuilder{results: map[string]ctxbuilder.ProgramResult{
		"ACCTUPD": {
			Program: "ACCTUPD",
			Document: &kb.ProgramDoc{
				Inputs:  []string{"INFILE"},
				Outputs: []string{"OUTFILE"},
				Calls:   []string{"BALANCE"},
			},
		},
		"BALANCE": {
			Program: "BALANCE",
			Document: &kb.ProgramDoc{
				Outputs: []string{"REPORT"},
			},
		},
	}}
	tmp := t.TempDir()
	target := filepath.Join(tmp, "spring")
	gen := NewSpringGenerator(meta, builder, func(string, string, ...interface{}) {})
	path, err := gen.Generate(ctx, Options{ProjectID: "proj-123", TargetDir: target})
	if err != nil {
		t.Fatalf("generate: %v", err)
	}
	if path != target {
		t.Fatalf("expected generator to use provided directory, got %s", path)
	}
	checkExists(t, path, "pom.xml")
	checkExists(t, path, "mvnw")
	packagePath := filepath.Join("com", "katral", "proj-123")
	checkExists(t, path, filepath.Join("src", "main", "java", packagePath, "Application.java"))
	catalogPath := filepath.Join(path, "src", "main", "java", packagePath, "service", "ProgramCatalogService.java")
	data, err := os.ReadFile(catalogPath)
	if err != nil {
		t.Fatalf("read catalog service: %v", err)
	}
	content := string(data)
	if !strings.Contains(content, "ProgramSummary(\"ACCTUPD\"") {
		t.Fatalf("expected catalog to reference ACCTUPD, got %s", content)
	}
	if !strings.Contains(content, "List.of(\"INFILE\"") {
		t.Fatalf("expected catalog to include inputs, got %s", content)
	}
	if !strings.Contains(content, "\"COBOL\"") || !strings.Contains(content, "\"CICS\"") {
		t.Fatalf("expected catalog to include technologies, got %s", content)
	}
}

func checkExists(t *testing.T, root, rel string) {
	t.Helper()
	path := filepath.Join(root, rel)
	if _, err := os.Stat(path); err != nil {
		t.Fatalf("expected %s to exist: %v", rel, err)
	}
}

type fakeMetadataStore struct {
	programs []metadata.ProgramRecord
}

func (f *fakeMetadataStore) QueryPrograms(context.Context, metadata.QueryOptions) (metadata.ProgramsPage, error) {
	return metadata.ProgramsPage{}, nil
}

func (f *fakeMetadataStore) StreamPrograms(ctx context.Context, opts metadata.QueryOptions, fn func(metadata.ProgramRecord) error) error {
	for _, rec := range f.programs {
		if opts.ProjectID != "" && !strings.EqualFold(rec.ProjectID, opts.ProjectID) {
			continue
		}
		if err := fn(rec); err != nil {
			return err
		}
	}
	return nil
}

func (f *fakeMetadataStore) TechnologyUsage(context.Context, string) ([]metadata.TechnologyUsage, error) {
	return nil, nil
}

func (f *fakeMetadataStore) DependencyCounts(context.Context, string, *int64) ([]metadata.DependencyCount, error) {
	return nil, nil
}

func (f *fakeMetadataStore) ChangeHistory(context.Context, int64, int) ([]metadata.ChangeEvent, error) {
	return nil, nil
}

func (f *fakeMetadataStore) CrossReferences(context.Context, metadata.CrossReferenceOptions) ([]metadata.CrossReference, error) {
	return nil, nil
}

func (f *fakeMetadataStore) BatchUpsertPrograms(context.Context, []metadata.ProgramUpsert) error {
	return nil
}

func (f *fakeMetadataStore) BatchUpsertFiles(context.Context, []metadata.FileUpsert) error {
	return nil
}

type fakeBuilder struct {
	results map[string]ctxbuilder.ProgramResult
}

func (f *fakeBuilder) BuildGoalContext(context.Context, ctxbuilder.GoalRequest) (ctxbuilder.GoalResult, error) {
	return ctxbuilder.GoalResult{}, nil
}

func (f *fakeBuilder) BuildProgramContext(ctx context.Context, req ctxbuilder.ProgramRequest) (ctxbuilder.ProgramResult, error) {
	if res, ok := f.results[strings.ToUpper(req.Program)]; ok {
		return res, nil
	}
	return ctxbuilder.ProgramResult{Program: req.Program}, nil
}

func (f *fakeBuilder) GraphDependencies() graph.DependencyService {
	return graph.NoopDependencyService()
}
