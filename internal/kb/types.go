// File path: internal/kb/types.go
package kb

import "github.com/nicodishanthj/Katral_phase1/internal/kb/model"

// Doc represents a knowledge base document chunk or metadata entry.
type Doc struct {
	ID           string             `json:"id"`
	Program      string             `json:"program"`
	SourcePath   string             `json:"source_path"`
	ProjectID    string             `json:"project_id,omitempty"`
	Chunk        int                `json:"chunk"`
	Type         string             `json:"type"`
	Content      string             `json:"content"`
	Summary      string             `json:"summary,omitempty"`
	Technologies []string           `json:"technologies,omitempty"`
	Inputs       []string           `json:"inputs,omitempty"`
	Outputs      []string           `json:"outputs,omitempty"`
	Calls        []string           `json:"calls,omitempty"`
	Paragraphs   []string           `json:"paragraphs,omitempty"`
	Logic        []string           `json:"logic,omitempty"`
	LogicTree    []*model.LogicStep `json:"logic_tree,omitempty"`
	FlowDiagram  *FlowDiagram       `json:"flow_diagram,omitempty"`
	Extra        map[string]string  `json:"extra,omitempty"`
	Fingerprint  string             `json:"fingerprint,omitempty"`

	PreviousFingerprint string `json:"-"`
}

// ProgramDoc aggregates metadata for a COBOL program.
type ProgramDoc struct {
	Program                string             `json:"program"`
	SourcePath             string             `json:"source_path"`
	Technologies           []string           `json:"technologies,omitempty"`
	Inputs                 []string           `json:"inputs,omitempty"`
	Outputs                []string           `json:"outputs,omitempty"`
	Calls                  []string           `json:"calls,omitempty"`
	Paragraphs             []string           `json:"paragraphs,omitempty"`
	Logic                  []string           `json:"logic,omitempty"`
	LogicTree              []*model.LogicStep `json:"logic_tree,omitempty"`
	Flows                  []Doc              `json:"flows,omitempty"`
	WorkingNotes           []Doc              `json:"working_notes,omitempty"`
	BusinessRules          []Doc              `json:"business_rules,omitempty"`
	Modernization          []Doc              `json:"modernization,omitempty"`
	Chunks                 []Doc              `json:"chunks,omitempty"`
	DocumentationSummaries []Doc              `json:"documentation_summaries,omitempty"`
	CrossReferenceMaps     []Doc              `json:"cross_reference_maps,omitempty"`
	ImpactAssessments      []Doc              `json:"impact_assessments,omitempty"`
	ConversionSummaries    []Doc              `json:"conversion_summaries,omitempty"`
	ConversionMappings     []Doc              `json:"conversion_mappings,omitempty"`
	ConversionSources      []Doc              `json:"conversion_sources,omitempty"`
	FlowPrompts            []Doc              `json:"documentation_program_flows,omitempty"`
	BusinessRulePrompts    []Doc              `json:"documentation_business_rules,omitempty"`
	FunctionalPrompts      []Doc              `json:"documentation_functional_specs,omitempty"`
	TechnicalPrompts       []Doc              `json:"documentation_technical_specs,omitempty"`
	MigrationAssessments   []Doc              `json:"documentation_migration_assessments,omitempty"`
	ComponentMappings      []Doc              `json:"documentation_component_mappings,omitempty"`
	APICompatibility       []Doc              `json:"documentation_api_compatibility_matrices,omitempty"`
	MigrationTimelines     []Doc              `json:"documentation_migration_timelines,omitempty"`
	MigrationComplexities  []Doc              `json:"documentation_migration_complexities,omitempty"`
	PatternRecommendations []Doc              `json:"documentation_pattern_recommendations,omitempty"`
	MigratedCodeReviews    []Doc              `json:"documentation_migrated_code_reviews,omitempty"`
	PerformanceComparisons []Doc              `json:"documentation_performance_comparisons,omitempty"`
	Dependencies           []GraphNeighbor    `json:"dependencies,omitempty"`
	Impacts                []GraphNeighbor    `json:"impacts,omitempty"`
	Related                []GraphNeighbor    `json:"related,omitempty"`
}

// ProjectDoc describes a project overview.
type ProjectDoc struct {
	Programs []ProgramDoc `json:"programs"`
}

// FlowDiagram describes a directed graph for visualizing program logic.
type FlowDiagram struct {
	Nodes []FlowNode `json:"nodes"`
	Edges []FlowEdge `json:"edges"`
}

// FlowNode represents a step in a program's control flow.
type FlowNode struct {
	ID    string `json:"id"`
	Label string `json:"label"`
	Type  string `json:"type,omitempty"`
}

// FlowEdge connects two nodes in the control-flow graph.
type FlowEdge struct {
	From  string `json:"from"`
	To    string `json:"to"`
	Label string `json:"label,omitempty"`
}

// GraphNeighbor describes an associated program in the graph context.
type GraphNeighbor struct {
	Program  string   `json:"program"`
	Name     string   `json:"name,omitempty"`
	Source   string   `json:"source,omitempty"`
	Distance int      `json:"distance,omitempty"`
	Weight   float64  `json:"weight,omitempty"`
	Chain    []string `json:"chain,omitempty"`
	Kind     string   `json:"kind,omitempty"`
}
