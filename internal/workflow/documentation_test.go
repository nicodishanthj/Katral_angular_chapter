// File path: internal/workflow/documentation_test.go
package workflow

import (
	"archive/zip"
	"context"
	"os"
	"strings"
	"testing"
	"time"

	ctxbuilder "github.com/nicodishanthj/Katral_phase1/internal/context"
	"github.com/nicodishanthj/Katral_phase1/internal/graph"
	"github.com/nicodishanthj/Katral_phase1/internal/kb"
	"github.com/nicodishanthj/Katral_phase1/internal/memory"
	"github.com/nicodishanthj/Katral_phase1/internal/metadata"
	"github.com/nicodishanthj/Katral_phase1/internal/retriever"
)

func TestSummarizeDocumentationGeneratesArtifactsIncrementally(t *testing.T) {
	ctx := context.Background()
	store, err := memory.NewStore(t.TempDir())
	if err != nil {
		t.Fatalf("new store: %v", err)
	}
	projectID := "proj-1"
	baseDoc := kb.Doc{ID: "PGM1:metadata", Program: "PGM1", Type: "metadata", SourcePath: "pgm1.cbl"}
	if err := store.ReplaceDocs(ctx, projectID, []kb.Doc{baseDoc}); err != nil {
		t.Fatalf("seed docs: %v", err)
	}
	docs, err := store.AllDocs(ctx, "")
	if err != nil {
		t.Fatalf("load docs: %v", err)
	}
	retr := retriever.New(docs, nil)

	updatedAt := time.Now().Add(-time.Hour)
	metadataStore := &stubMetadataStore{
		programs: []metadata.ProgramRecord{{
			ID:         1,
			ProjectID:  projectID,
			Name:       "PGM1",
			SourcePath: "pgm1.cbl",
			Summary:    "Handles account updates",
			UpdatedAt:  updatedAt,
			LastDocumentUpdate: func() *time.Time {
				ts := updatedAt
				return &ts
			}(),
			DocumentCount: 4,
			DocTypeCount:  3,
			Technologies:  []string{"COBOL", "CICS"},
		}},
		counts: map[int64][]metadata.DependencyCount{
			1: {{ProgramID: 1, Kind: "CALL", Count: 2}},
		},
		crossRefs: map[string][]metadata.CrossReference{
			"PGM1": {{ProgramID: 2, Program: "PGM2", Kind: "CALL", Target: "PGM1"}},
		},
	}

	builder := newStubBuilder()
	builder.setResult("PGM1", ctxbuilder.ProgramResult{
		Program: "PGM1",
		Document: &kb.ProgramDoc{
			Program:      "PGM1",
			Inputs:       []string{"INFILE"},
			Outputs:      []string{"OUTFILE"},
			Calls:        []string{"PGM2"},
			Paragraphs:   []string{"INIT-ACCT", "UPDATE-ACCT"},
			Logic:        []string{"Initialize account context", "Apply transaction updates"},
			WorkingNotes: []kb.Doc{{Summary: "Ensure nightly batch runs before submitting updates."}},
			BusinessRules: []kb.Doc{{
				Content: "Business rules inferred for PGM1:\n- Validate account status before updates\n- Reject duplicate transactions",
			}},
			Flows: []kb.Doc{{
				Content: "- Start account update\n- Apply validation checks\n- Persist results",
			}},
		},
		Graph: ctxbuilder.GraphContext{
			Program:      "PGM1",
			Dependencies: []kb.GraphNeighbor{{Program: "PGM2", Chain: []string{"PGM1", "PGM2"}, Distance: 1}},
			Impacts:      []kb.GraphNeighbor{{Program: "PGM0", Chain: []string{"PGM0", "PGM1"}, Distance: 1}},
			Related:      []kb.GraphNeighbor{{Program: "PGM3"}},
		},
	})

	mgr := NewManager(store, nil, retr, builder, nil, nil, graph.NoopDependencyService(), metadataStore, nil, nil)

	mgr.workflowMu.Lock()
	mgr.workflows[projectID] = &session{state: State{Request: Request{ProjectID: projectID}}}
	mgr.workflowMu.Unlock()
	defer func() {
		mgr.workflowMu.Lock()
		delete(mgr.workflows, projectID)
		mgr.workflowMu.Unlock()
	}()

	summary, err := mgr.summarizeDocumentation(ctx, projectID)
	if err != nil {
		t.Fatalf("summarize documentation: %v", err)
	}
	if builder.callCount("PGM1") != 1 {
		t.Fatalf("expected builder call count 1, got %d", builder.callCount("PGM1"))
	}
	if want := "documentation summaries"; !contains(summary, want) {
		t.Fatalf("summary missing %q: %s", want, summary)
	}
	for _, want := range []string{"program flow prompts", "business rule prompts", "functional specification prompts", "technical specification prompts"} {
		if !contains(summary, want) {
			t.Fatalf("summary missing %q: %s", want, summary)
		}
	}

	generated, err := store.AllDocs(ctx, projectID)
	if err != nil {
		t.Fatalf("load generated docs: %v", err)
	}
	var crossFound, impactFound bool
	var summaryDoc kb.Doc
	var flowPromptDoc, businessPromptDoc, functionalPromptDoc, technicalPromptDoc kb.Doc
	for _, doc := range generated {
		switch doc.Type {
		case docTypeCrossReference:
			crossFound = true
			if !contains(doc.Content, "PGM2") {
				t.Fatalf("expected cross reference to include PGM2, got %q", doc.Content)
			}
		case docTypeImpact:
			impactFound = true
			if !contains(doc.Content, "CALL") {
				t.Fatalf("expected impact doc to include dependency counts, got %q", doc.Content)
			}
		case docTypeSummary:
			summaryDoc = doc
		case docTypeProgramFlow:
			flowPromptDoc = doc
		case docTypeBusinessPrompt:
			businessPromptDoc = doc
		case docTypeFunctionalSpec:
			functionalPromptDoc = doc
		case docTypeTechnicalSpec:
			technicalPromptDoc = doc
		}
	}
	if !crossFound || !impactFound {
		t.Fatalf("expected generated cross and impact docs; found cross=%v impact=%v", crossFound, impactFound)
	}
	if summaryDoc.ID == "" {
		t.Fatalf("expected summary documentation to be generated")
	}
	if flowPromptDoc.ID == "" {
		t.Fatalf("expected program flow prompt to be generated")
	}
	if businessPromptDoc.ID == "" {
		t.Fatalf("expected business rule prompt to be generated")
	}
	if functionalPromptDoc.ID == "" {
		t.Fatalf("expected functional specification prompt to be generated")
	}
	if technicalPromptDoc.ID == "" {
		t.Fatalf("expected technical specification prompt to be generated")
	}
	if !contains(summaryDoc.Content, "technical specifications") {
		t.Fatalf("expected technical specifications section in summary doc: %q", summaryDoc.Content)
	}
	if !contains(summaryDoc.Content, "inputs: infile") {
		t.Fatalf("expected inputs listed in technical specifications: %q", summaryDoc.Content)
	}
	if !contains(summaryDoc.Content, "ensure nightly batch runs before submitting updates") {
		t.Fatalf("expected working notes in functional specifications: %q", summaryDoc.Content)
	}
	if !contains(summaryDoc.Content, "validate account status before updates") {
		t.Fatalf("expected business rules in summary doc: %q", summaryDoc.Content)
	}
	if !contains(flowPromptDoc.Content, "Program Flow Prompt Template") || !contains(flowPromptDoc.Content, "Guidance:") {
		t.Fatalf("expected program flow prompt content to include guidance: %q", flowPromptDoc.Content)
	}
	if !contains(businessPromptDoc.Content, "Business Rules Prompt Template") || !contains(businessPromptDoc.Content, "Data constraints") {
		t.Fatalf("expected business rule prompt to highlight constraints: %q", businessPromptDoc.Content)
	}
	if !contains(functionalPromptDoc.Content, "Functional Specification Prompt Template") || !contains(functionalPromptDoc.Content, "Functional responsibilities") {
		t.Fatalf("expected functional prompt structure: %q", functionalPromptDoc.Content)
	}
	if !contains(technicalPromptDoc.Content, "Technical Specification Prompt Template") || !contains(technicalPromptDoc.Content, "Data model considerations") {
		t.Fatalf("expected technical prompt to mention data model considerations: %q", technicalPromptDoc.Content)
	}

	mgr.workflowMu.Lock()
	artifacts := mgr.workflows[projectID].state.DocumentationArtifacts
	mgr.workflowMu.Unlock()
	if len(artifacts) == 0 {
		t.Fatalf("expected documentation artifacts to be recorded")
	}
	for _, kind := range []string{
		docTypeSummary,
		docTypeCrossReference,
		docTypeImpact,
		docTypeProgramFlow,
		docTypeBusinessPrompt,
		docTypeFunctionalSpec,
		docTypeTechnicalSpec,
	} {
		path, ok := artifacts[kind]
		if !ok {
			t.Fatalf("expected artifact for %s", kind)
		}
		if _, err := os.Stat(path); err != nil {
			t.Fatalf("expected artifact file for %s: %v", kind, err)
		}
	}

	summaryArchive := artifacts["documentation_summary"]
	reader, err := zip.OpenReader(summaryArchive)
	if err != nil {
		t.Fatalf("open summary archive: %v", err)
	}
	var manifestFound bool
	for _, f := range reader.File {
		if f.Name == "manifest.json" {
			manifestFound = true
			break
		}
	}
	_ = reader.Close()
	if !manifestFound {
		t.Fatalf("expected manifest.json in documentation summary archive")
	}

	// Second run should be incremental and skip builder invocation.
	summary, err = mgr.summarizeDocumentation(ctx, projectID)
	if err != nil {
		t.Fatalf("second summarize: %v", err)
	}
	if builder.callCount("PGM1") != 1 {
		t.Fatalf("expected builder not invoked on second run, got %d calls", builder.callCount("PGM1"))
	}

	// Update metadata version to force regeneration.
	newVersion := time.Now()
	metadataStore.programs[0].LastDocumentUpdate = &newVersion
	summary, err = mgr.summarizeDocumentation(ctx, projectID)
	if err != nil {
		t.Fatalf("third summarize: %v", err)
	}
	if builder.callCount("PGM1") != 2 {
		t.Fatalf("expected builder invoked after metadata change, got %d calls", builder.callCount("PGM1"))
	}
	if want := "impact assessments"; !contains(summary, want) {
		t.Fatalf("summary missing updated metric %q: %s", want, summary)
	}
}

func contains(s, substr string) bool {
	return strings.Contains(strings.ToLower(s), strings.ToLower(substr))
}

type stubMetadataStore struct {
	programs  []metadata.ProgramRecord
	counts    map[int64][]metadata.DependencyCount
	crossRefs map[string][]metadata.CrossReference
}

func (s *stubMetadataStore) QueryPrograms(context.Context, metadata.QueryOptions) (metadata.ProgramsPage, error) {
	return metadata.ProgramsPage{}, nil
}

func (s *stubMetadataStore) StreamPrograms(ctx context.Context, opts metadata.QueryOptions, fn func(metadata.ProgramRecord) error) error {
	for _, rec := range s.programs {
		if strings.TrimSpace(opts.ProjectID) != "" && !strings.EqualFold(rec.ProjectID, opts.ProjectID) {
			continue
		}
		if err := fn(rec); err != nil {
			return err
		}
	}
	return nil
}

func (s *stubMetadataStore) TechnologyUsage(context.Context, string) ([]metadata.TechnologyUsage, error) {
	return nil, nil
}

func (s *stubMetadataStore) DependencyCounts(ctx context.Context, projectID string, programID *int64) ([]metadata.DependencyCount, error) {
	if programID == nil {
		return nil, nil
	}
	if counts, ok := s.counts[*programID]; ok {
		return counts, nil
	}
	return nil, nil
}

func (s *stubMetadataStore) ChangeHistory(context.Context, int64, int) ([]metadata.ChangeEvent, error) {
	return nil, nil
}

func (s *stubMetadataStore) CrossReferences(ctx context.Context, opts metadata.CrossReferenceOptions) ([]metadata.CrossReference, error) {
	key := strings.ToUpper(strings.TrimSpace(opts.Target))
	if refs, ok := s.crossRefs[key]; ok {
		return refs, nil
	}
	return nil, nil
}

func (s *stubMetadataStore) BatchUpsertPrograms(context.Context, []metadata.ProgramUpsert) error {
	return nil
}
func (s *stubMetadataStore) BatchUpsertFiles(context.Context, []metadata.FileUpsert) error {
	return nil
}

type stubBuilder struct {
	results map[string]ctxbuilder.ProgramResult
	calls   map[string]int
}

func newStubBuilder() *stubBuilder {
	return &stubBuilder{results: make(map[string]ctxbuilder.ProgramResult), calls: make(map[string]int)}
}

func (s *stubBuilder) setResult(program string, result ctxbuilder.ProgramResult) {
	s.results[strings.ToUpper(strings.TrimSpace(program))] = result
}

func (s *stubBuilder) callCount(program string) int {
	return s.calls[strings.ToUpper(strings.TrimSpace(program))]
}

func (s *stubBuilder) BuildGoalContext(context.Context, ctxbuilder.GoalRequest) (ctxbuilder.GoalResult, error) {
	return ctxbuilder.GoalResult{}, nil
}

func (s *stubBuilder) BuildProgramContext(_ context.Context, req ctxbuilder.ProgramRequest) (ctxbuilder.ProgramResult, error) {
	key := strings.ToUpper(strings.TrimSpace(req.Program))
	s.calls[key]++
	if result, ok := s.results[key]; ok {
		return result, nil
	}
	return ctxbuilder.ProgramResult{Program: key}, nil
}

func (s *stubBuilder) GraphDependencies() graph.DependencyService {
	return graph.NoopDependencyService()
}
