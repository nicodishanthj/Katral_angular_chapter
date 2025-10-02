// File path: internal/workflow/generator_integration_test.go
package workflow

import (
	"context"
	"os"
	"path/filepath"
	"testing"

	ctxbuilder "github.com/nicodishanthj/Katral_phase1/internal/context"
	"github.com/nicodishanthj/Katral_phase1/internal/graph"
	"github.com/nicodishanthj/Katral_phase1/internal/kb"
	"github.com/nicodishanthj/Katral_phase1/internal/memory"
	"github.com/nicodishanthj/Katral_phase1/internal/metadata"
	"github.com/nicodishanthj/Katral_phase1/internal/retriever"
)

func TestGenerateAndVerifySpringProject(t *testing.T) {
	ctx := context.Background()
	tmp := t.TempDir()
	store, err := memory.NewStore(filepath.Join(tmp, "docs.jsonl"))
	if err != nil {
		t.Fatalf("new store: %v", err)
	}
	meta := &workflowMetadataStore{programs: []metadata.ProgramRecord{{
		ProjectID:  "proj-123",
		Name:       "ACCTUPD",
		SourcePath: "ACCTUPD.cbl",
		Summary:    "Handles account updates",
	}}}
	builder := &workflowStubBuilder{results: map[string]ctxbuilder.ProgramResult{
		"ACCTUPD": {
			Program: "ACCTUPD",
			Document: &kb.ProgramDoc{
				Inputs:  []string{"INFILE"},
				Outputs: []string{"OUTFILE"},
			},
		},
	}}
	retr := retriever.New(nil, nil)
	mgr := NewManager(store, nil, retr, builder, nil, nil, graph.NoopDependencyService(), meta, nil, nil)

	path, err := mgr.generateSpringProject(ctx, "proj-123", Request{})
	if err != nil {
		t.Fatalf("generate spring project: %v", err)
	}
	if path == "" {
		t.Fatalf("expected generator to return project path")
	}
	if _, err := os.Stat(filepath.Join(path, "pom.xml")); err != nil {
		t.Fatalf("expected pom.xml: %v", err)
	}
	if err := mgr.verifySpringProject(ctx, Request{SpringProject: path}); err != nil {
		t.Fatalf("verify spring project: %v", err)
	}
	artifact, err := mgr.packageSpringProject(ctx, "proj-123", path)
	if err != nil {
		t.Fatalf("package spring project: %v", err)
	}
	if _, err := os.Stat(artifact); err != nil {
		t.Fatalf("expected artifact to exist: %v", err)
	}
}

type workflowMetadataStore struct {
	programs []metadata.ProgramRecord
}

func (s *workflowMetadataStore) QueryPrograms(context.Context, metadata.QueryOptions) (metadata.ProgramsPage, error) {
	return metadata.ProgramsPage{}, nil
}

func (s *workflowMetadataStore) StreamPrograms(ctx context.Context, opts metadata.QueryOptions, fn func(metadata.ProgramRecord) error) error {
	for _, rec := range s.programs {
		if opts.ProjectID != "" && rec.ProjectID != opts.ProjectID {
			continue
		}
		if err := fn(rec); err != nil {
			return err
		}
	}
	return nil
}

func (s *workflowMetadataStore) TechnologyUsage(context.Context, string) ([]metadata.TechnologyUsage, error) {
	return nil, nil
}

func (s *workflowMetadataStore) DependencyCounts(context.Context, string, *int64) ([]metadata.DependencyCount, error) {
	return nil, nil
}

func (s *workflowMetadataStore) ChangeHistory(context.Context, int64, int) ([]metadata.ChangeEvent, error) {
	return nil, nil
}

func (s *workflowMetadataStore) CrossReferences(context.Context, metadata.CrossReferenceOptions) ([]metadata.CrossReference, error) {
	return nil, nil
}

func (s *workflowMetadataStore) BatchUpsertPrograms(context.Context, []metadata.ProgramUpsert) error {
	return nil
}

func (s *workflowMetadataStore) BatchUpsertFiles(context.Context, []metadata.FileUpsert) error {
	return nil
}

type workflowStubBuilder struct {
	results map[string]ctxbuilder.ProgramResult
}

func (b *workflowStubBuilder) BuildGoalContext(context.Context, ctxbuilder.GoalRequest) (ctxbuilder.GoalResult, error) {
	return ctxbuilder.GoalResult{}, nil
}

func (b *workflowStubBuilder) BuildProgramContext(ctx context.Context, req ctxbuilder.ProgramRequest) (ctxbuilder.ProgramResult, error) {
	if res, ok := b.results[req.Program]; ok {
		return res, nil
	}
	return ctxbuilder.ProgramResult{Program: req.Program}, nil
}

func (b *workflowStubBuilder) GraphDependencies() graph.DependencyService {
	return graph.NoopDependencyService()
}
