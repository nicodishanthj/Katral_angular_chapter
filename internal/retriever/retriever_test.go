// File path: internal/retriever/retriever_test.go
package retriever

import (
	"context"
	"testing"

	"github.com/nicodishanthj/Katral_phase1/internal/kb"
)

func TestProjectDocIncludesExtendedFamilies(t *testing.T) {
	docs := []kb.Doc{
		{
			ID:           "prog1:metadata",
			Program:      "PROG1",
			SourcePath:   "src/prog1.cob",
			ProjectID:    "proj-a",
			Type:         "metadata",
			Technologies: []string{"COBOL"},
			Inputs:       []string{"IN-FILE"},
			Outputs:      []string{"OUT-FILE"},
			Calls:        []string{"SUB1"},
			Paragraphs:   []string{"MAIN-LOGIC"},
			Logic:        []string{"IF X = Y"},
		},
		{
			ID:         "prog1:flow:0",
			Program:    "PROG1",
			SourcePath: "src/prog1.cob",
			ProjectID:  "proj-a",
			Type:       "flow",
			Content:    "Derived flow graph",
		},
		{
			ID:         "prog1:note:0",
			Program:    "PROG1",
			SourcePath: "src/prog1.cob",
			ProjectID:  "proj-a",
			Type:       "working_notes",
			Content:    "Implementation note",
		},
		{
			ID:         "prog1:rules:0",
			Program:    "PROG1",
			SourcePath: "src/prog1.cob",
			ProjectID:  "proj-a",
			Type:       "business_rules",
			Content:    "Validate account",
		},
		{
			ID:         "prog1:modernization",
			Program:    "PROG1",
			SourcePath: "modernization",
			ProjectID:  "proj-a",
			Type:       "modernization",
			Content:    "Rewrite recommendation",
		},
		{
			ID:         "prog1:chunk:0",
			Program:    "PROG1",
			SourcePath: "src/prog1.cob",
			Chunk:      0,
			ProjectID:  "proj-a",
			Type:       "chunk",
			Content:    "IDENTIFICATION DIVISION.",
		},
		{
			ID:        "prog1:doc-summary",
			Program:   "PROG1",
			ProjectID: "proj-a",
			Type:      "documentation_summary",
			Summary:   "Program overview",
		},
		{
			ID:        "prog1:doc-cross",
			Program:   "PROG1",
			ProjectID: "proj-a",
			Type:      "documentation_cross_reference",
			Content:   "Calls SUB1",
		},
		{
			ID:        "prog1:doc-impact",
			Program:   "PROG1",
			ProjectID: "proj-a",
			Type:      "documentation_impact_analysis",
			Content:   "Impacts customer flows",
		},
		{
			ID:        "prog1:conv-summary",
			Program:   "PROG1",
			ProjectID: "proj-a",
			Type:      "conversion_summary",
			Summary:   "Converted service overview",
		},
		{
			ID:        "prog1:conv-mapping",
			Program:   "PROG1",
			ProjectID: "proj-a",
			Type:      "conversion_mapping",
			Content:   "Map COBOL routines to Spring beans",
		},
		{
			ID:        "prog1:conv-source",
			Program:   "PROG1",
			ProjectID: "proj-a",
			Type:      "conversion_source",
			Content:   "Converted Java source archive",
		},
	}

	r := New(docs, nil)
	project := r.ProjectDoc(context.Background(), "proj-a", nil, nil)
	if len(project.Programs) != 1 {
		t.Fatalf("expected 1 program, got %d", len(project.Programs))
	}

	program := project.Programs[0]
	if len(program.Flows) != 1 {
		t.Fatalf("expected flow docs to be aggregated, got %d", len(program.Flows))
	}
	if len(program.WorkingNotes) != 1 {
		t.Fatalf("expected working notes to be aggregated, got %d", len(program.WorkingNotes))
	}
	if len(program.BusinessRules) != 1 {
		t.Fatalf("expected business rules to be aggregated, got %d", len(program.BusinessRules))
	}
	if len(program.Modernization) != 1 {
		t.Fatalf("expected modernization docs to be aggregated, got %d", len(program.Modernization))
	}
	if len(program.Chunks) != 1 {
		t.Fatalf("expected chunk docs to be aggregated, got %d", len(program.Chunks))
	}
	if len(program.DocumentationSummaries) != 1 {
		t.Fatalf("expected documentation summaries to be aggregated, got %d", len(program.DocumentationSummaries))
	}
	if len(program.CrossReferenceMaps) != 1 {
		t.Fatalf("expected cross-reference docs to be aggregated, got %d", len(program.CrossReferenceMaps))
	}
	if len(program.ImpactAssessments) != 1 {
		t.Fatalf("expected impact docs to be aggregated, got %d", len(program.ImpactAssessments))
	}
	if len(program.ConversionSummaries) != 1 {
		t.Fatalf("expected conversion summaries to be aggregated, got %d", len(program.ConversionSummaries))
	}
	if len(program.ConversionMappings) != 1 {
		t.Fatalf("expected conversion mappings to be aggregated, got %d", len(program.ConversionMappings))
	}
	if len(program.ConversionSources) != 1 {
		t.Fatalf("expected conversion sources to be aggregated, got %d", len(program.ConversionSources))
	}
	if got := program.Technologies; len(got) != 1 || got[0] != "COBOL" {
		t.Fatalf("metadata fields were not carried through: %+v", program.Technologies)
	}
}

func TestProgramDocLookupIsCaseInsensitive(t *testing.T) {
	docs := []kb.Doc{
		{
			ID:         "prog2:meta",
			Program:    "PROG2",
			SourcePath: "src/prog2.cob",
			ProjectID:  "proj-b",
			Type:       "metadata",
		},
		{
			ID:         "prog2:flow:0",
			Program:    "PROG2",
			SourcePath: "src/prog2.cob",
			ProjectID:  "proj-b",
			Type:       "flow",
			Content:    "Flow",
		},
	}

	r := New(docs, nil)
	program, ok := r.ProgramDoc(context.Background(), "proj-b", "prog2", nil, nil)
	if !ok {
		t.Fatalf("expected program doc lookup to succeed")
	}
	if program.Program != "PROG2" {
		t.Fatalf("expected normalized program identifier, got %q", program.Program)
	}
	if len(program.Flows) != 1 {
		t.Fatalf("expected flow docs to be present, got %d", len(program.Flows))
	}
}

func TestProjectScopedAggregationFiltersDocs(t *testing.T) {
	docs := []kb.Doc{
		{ID: "proga:meta", Program: "PROGA", ProjectID: "proj-a", Type: "metadata"},
		{ID: "progb:meta", Program: "PROGB", ProjectID: "proj-b", Type: "metadata"},
		{ID: "progb:flow:0", Program: "PROGB", ProjectID: "proj-b", Type: "flow"},
	}

	r := New(docs, nil)

	projectA := r.ProjectDoc(context.Background(), "proj-a", nil, nil)
	if len(projectA.Programs) != 1 {
		t.Fatalf("expected only project A program, got %d", len(projectA.Programs))
	}
	if projectA.Programs[0].Program != "PROGA" {
		t.Fatalf("unexpected program for project A: %+v", projectA.Programs[0])
	}

	if _, ok := r.ProgramDoc(context.Background(), "proj-a", "PROGB", nil, nil); ok {
		t.Fatalf("expected project B program to be hidden from project A")
	}

	programB, ok := r.ProgramDoc(context.Background(), "proj-b", "PROGB", nil, nil)
	if !ok {
		t.Fatalf("expected project B program to be retrievable")
	}
	if len(programB.Flows) != 1 || programB.Flows[0].ID != "progb:flow:0" {
		t.Fatalf("expected project B flow aggregation, got %+v", programB.Flows)
	}

	empty := r.ProjectDoc(context.Background(), "proj-missing", nil, nil)
	if len(empty.Programs) != 0 {
		t.Fatalf("expected no programs for unknown project, got %d", len(empty.Programs))
	}
}
