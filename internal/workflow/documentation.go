// File path: internal/workflow/documentation.go
package workflow

import (
	"context"
	"fmt"
	"sort"
	"strings"
	"time"
	"unicode/utf8"

	"github.com/nicodishanthj/Katral_phase1/internal/common"
	ctxbuilder "github.com/nicodishanthj/Katral_phase1/internal/context"
	"github.com/nicodishanthj/Katral_phase1/internal/kb"
	"github.com/nicodishanthj/Katral_phase1/internal/metadata"
)

const (
	docTypeSummary             = "documentation_summary"
	docTypeCrossReference      = "documentation_cross_reference"
	docTypeImpact              = "documentation_impact_analysis"
	docTypeProgramFlow         = "documentation_program_flow"
	docTypeBusinessPrompt      = "documentation_business_rules"
	docTypeFunctionalSpec      = "documentation_functional_spec"
	docTypeTechnicalSpec       = "documentation_technical_spec"
	docTypeMigrationAssessment = "documentation_migration_assessment"
	docTypeComponentMapping    = "documentation_component_mapping"
	docTypeAPICompatibility    = "documentation_api_compatibility"
	docTypeMigrationTimeline   = "documentation_migration_timeline"
	metadataVersionKey         = "metadata_version"
)

type documentationTemplate struct {
	Program      string
	Source       string
	Summary      string
	Technologies []string
	Inputs       []string
	Outputs      []string
	Calls        []string
	FlowSteps    []string
	Functional   []string
	Business     []string
	Technical    []string
	DataModels   []string
	DTOs         []string
}

type templateSection struct {
	Title string
	Lines []string
}

// enrichDocumentation synthesises documentation artifacts for each program using
// the context builder and metadata catalog. The returned slice represents the
// complete document set for the project, while the boolean indicates whether any
// artifacts changed as part of the refresh.
func (m *Manager) enrichDocumentation(ctx context.Context, projectID string, docs []kb.Doc) ([]kb.Doc, bool, error) {
	if m.ctx == nil || m.metadata == nil {
		return docs, false, nil
	}
	logger := common.Logger()

	// Index the existing documentation artifacts for quick lookups.
	existing := make(map[string]kb.Doc, len(docs))
	for _, doc := range docs {
		existing[doc.ID] = doc
	}

	var generated []kb.Doc
	changed := false
	managedTypes := map[string]struct{}{
		docTypeSummary:             {},
		docTypeCrossReference:      {},
		docTypeImpact:              {},
		docTypeProgramFlow:         {},
		docTypeBusinessPrompt:      {},
		docTypeFunctionalSpec:      {},
		docTypeTechnicalSpec:       {},
		docTypeMigrationAssessment: {},
		docTypeComponentMapping:    {},
		docTypeAPICompatibility:    {},
		docTypeMigrationTimeline:   {},
	}

	buildErr := m.metadata.StreamPrograms(ctx, metadata.QueryOptions{ProjectID: projectID}, func(record metadata.ProgramRecord) error {
		program := strings.TrimSpace(record.Name)
		if program == "" {
			return nil
		}
		version := formatMetadataVersion(record)
		summaryID := documentationDocID(program, docTypeSummary)
		crossID := documentationDocID(program, docTypeCrossReference)
		impactID := documentationDocID(program, docTypeImpact)
		flowID := documentationDocID(program, docTypeProgramFlow)
		businessID := documentationDocID(program, docTypeBusinessPrompt)
		functionalID := documentationDocID(program, docTypeFunctionalSpec)
		technicalID := documentationDocID(program, docTypeTechnicalSpec)
		assessmentID := documentationDocID(program, docTypeMigrationAssessment)
		mappingID := documentationDocID(program, docTypeComponentMapping)
		compatibilityID := documentationDocID(program, docTypeAPICompatibility)
		timelineID := documentationDocID(program, docTypeMigrationTimeline)

		summaryDoc, summaryExists := existing[summaryID]
		crossDoc, crossExists := existing[crossID]
		impactDoc, impactExists := existing[impactID]
		flowDoc, flowExists := existing[flowID]
		businessDoc, businessExists := existing[businessID]
		functionalDoc, functionalExists := existing[functionalID]
		technicalDoc, technicalExists := existing[technicalID]
		assessmentDoc, assessmentExists := existing[assessmentID]
		mappingDoc, mappingExists := existing[mappingID]
		compatibilityDoc, compatibilityExists := existing[compatibilityID]
		timelineDoc, timelineExists := existing[timelineID]

		if summaryExists && crossExists && impactExists && flowExists && businessExists && functionalExists && technicalExists && assessmentExists && mappingExists && compatibilityExists && timelineExists &&
			docMatchesVersion(summaryDoc, version) &&
			docMatchesVersion(crossDoc, version) &&
			docMatchesVersion(impactDoc, version) &&
			docMatchesVersion(flowDoc, version) &&
			docMatchesVersion(businessDoc, version) &&
			docMatchesVersion(functionalDoc, version) &&
			docMatchesVersion(technicalDoc, version) &&
			docMatchesVersion(assessmentDoc, version) &&
			docMatchesVersion(mappingDoc, version) &&
			docMatchesVersion(compatibilityDoc, version) &&
			docMatchesVersion(timelineDoc, version) {
			generated = append(generated, summaryDoc, crossDoc, impactDoc, flowDoc, businessDoc, functionalDoc, technicalDoc, assessmentDoc, mappingDoc, compatibilityDoc, timelineDoc)
			return nil
		}

		result, err := m.ctx.BuildProgramContext(ctx, ctxbuilder.ProgramRequest{Program: program, ProjectID: projectID})
		if err != nil {
			logger.Warn("workflow: program context build failed", "program", program, "error", err)
		}

		template := newDocumentationTemplate(record, result)

		summary := buildSummaryDoc(record, result)
		summary.ID = summaryID
		summary.Program = program
		summary.Type = docTypeSummary
		summary.Summary = fmt.Sprintf("Program overview for %s", program)
		summary.Extra = mergeExtra(summary.Extra, version)
		summary.Fingerprint = kb.ComputeFingerprint(summary)
		if !summaryExists || summary.Fingerprint != summaryDoc.Fingerprint {
			changed = true
		}
		generated = append(generated, summary)

		cross := buildCrossReferenceDoc(ctx, m.metadata, projectID, record, result)
		cross.ID = crossID
		cross.Program = program
		cross.Type = docTypeCrossReference
		cross.Summary = fmt.Sprintf("Cross-reference map for %s", program)
		cross.Extra = mergeExtra(cross.Extra, version)
		cross.Fingerprint = kb.ComputeFingerprint(cross)
		if !crossExists || cross.Fingerprint != crossDoc.Fingerprint {
			changed = true
		}
		generated = append(generated, cross)

		impact := buildImpactDoc(ctx, m.metadata, projectID, record, result)
		impact.ID = impactID
		impact.Program = program
		impact.Type = docTypeImpact
		impact.Summary = fmt.Sprintf("Impact assessment for %s", program)
		impact.Extra = mergeExtra(impact.Extra, version)
		impact.Fingerprint = kb.ComputeFingerprint(impact)
		if !impactExists || impact.Fingerprint != impactDoc.Fingerprint {
			changed = true
		}
		generated = append(generated, impact)

		flowPrompt := buildProgramFlowDoc(template)
		flowPrompt.ID = flowID
		flowPrompt.Program = program
		flowPrompt.Type = docTypeProgramFlow
		flowPrompt.SourcePath = template.Source
		if flowPrompt.SourcePath == "" {
			flowPrompt.SourcePath = record.SourcePath
		}
		flowPrompt.Summary = fmt.Sprintf("Program flow prompt for %s", program)
		flowPrompt.Extra = mergeExtra(flowPrompt.Extra, version)
		flowPrompt.Fingerprint = kb.ComputeFingerprint(flowPrompt)
		if !flowExists || flowPrompt.Fingerprint != flowDoc.Fingerprint {
			changed = true
		}
		generated = append(generated, flowPrompt)

		businessPrompt := buildBusinessRulesDoc(template)
		businessPrompt.ID = businessID
		businessPrompt.Program = program
		businessPrompt.Type = docTypeBusinessPrompt
		businessPrompt.SourcePath = template.Source
		if businessPrompt.SourcePath == "" {
			businessPrompt.SourcePath = record.SourcePath
		}
		businessPrompt.Summary = fmt.Sprintf("Business rule prompt for %s", program)
		businessPrompt.Extra = mergeExtra(businessPrompt.Extra, version)
		businessPrompt.Fingerprint = kb.ComputeFingerprint(businessPrompt)
		if !businessExists || businessPrompt.Fingerprint != businessDoc.Fingerprint {
			changed = true
		}
		generated = append(generated, businessPrompt)

		functionalPrompt := buildFunctionalSpecDoc(template)
		functionalPrompt.ID = functionalID
		functionalPrompt.Program = program
		functionalPrompt.Type = docTypeFunctionalSpec
		functionalPrompt.SourcePath = template.Source
		if functionalPrompt.SourcePath == "" {
			functionalPrompt.SourcePath = record.SourcePath
		}
		functionalPrompt.Summary = fmt.Sprintf("Functional specification prompt for %s", program)
		functionalPrompt.Extra = mergeExtra(functionalPrompt.Extra, version)
		functionalPrompt.Fingerprint = kb.ComputeFingerprint(functionalPrompt)
		if !functionalExists || functionalPrompt.Fingerprint != functionalDoc.Fingerprint {
			changed = true
		}
		generated = append(generated, functionalPrompt)

		technicalPrompt := buildTechnicalSpecDoc(template)
		technicalPrompt.ID = technicalID
		technicalPrompt.Program = program
		technicalPrompt.Type = docTypeTechnicalSpec
		technicalPrompt.SourcePath = template.Source
		if technicalPrompt.SourcePath == "" {
			technicalPrompt.SourcePath = record.SourcePath
		}
		technicalPrompt.Summary = fmt.Sprintf("Technical specification prompt for %s", program)
		technicalPrompt.Extra = mergeExtra(technicalPrompt.Extra, version)
		technicalPrompt.Fingerprint = kb.ComputeFingerprint(technicalPrompt)
		if !technicalExists || technicalPrompt.Fingerprint != technicalDoc.Fingerprint {
			changed = true
		}
		generated = append(generated, technicalPrompt)

		assessmentPrompt := buildMigrationAssessmentDoc(template)
		assessmentPrompt.ID = assessmentID
		assessmentPrompt.Program = program
		assessmentPrompt.Type = docTypeMigrationAssessment
		assessmentPrompt.SourcePath = template.Source
		if assessmentPrompt.SourcePath == "" {
			assessmentPrompt.SourcePath = record.SourcePath
		}
		assessmentPrompt.Summary = fmt.Sprintf("Migration assessment for %s", program)
		assessmentPrompt.Extra = mergeExtra(assessmentPrompt.Extra, version)
		assessmentPrompt.Fingerprint = kb.ComputeFingerprint(assessmentPrompt)
		if !assessmentExists || assessmentPrompt.Fingerprint != assessmentDoc.Fingerprint {
			changed = true
		}
		generated = append(generated, assessmentPrompt)

		mappingPrompt := buildComponentMappingDoc(template)
		mappingPrompt.ID = mappingID
		mappingPrompt.Program = program
		mappingPrompt.Type = docTypeComponentMapping
		mappingPrompt.SourcePath = template.Source
		if mappingPrompt.SourcePath == "" {
			mappingPrompt.SourcePath = record.SourcePath
		}
		mappingPrompt.Summary = fmt.Sprintf("Component mapping guidance for %s", program)
		mappingPrompt.Extra = mergeExtra(mappingPrompt.Extra, version)
		mappingPrompt.Fingerprint = kb.ComputeFingerprint(mappingPrompt)
		if !mappingExists || mappingPrompt.Fingerprint != mappingDoc.Fingerprint {
			changed = true
		}
		generated = append(generated, mappingPrompt)

		compatibilityPrompt := buildAPICompatibilityDoc(template)
		compatibilityPrompt.ID = compatibilityID
		compatibilityPrompt.Program = program
		compatibilityPrompt.Type = docTypeAPICompatibility
		compatibilityPrompt.SourcePath = template.Source
		if compatibilityPrompt.SourcePath == "" {
			compatibilityPrompt.SourcePath = record.SourcePath
		}
		compatibilityPrompt.Summary = fmt.Sprintf("API compatibility matrix for %s", program)
		compatibilityPrompt.Extra = mergeExtra(compatibilityPrompt.Extra, version)
		compatibilityPrompt.Fingerprint = kb.ComputeFingerprint(compatibilityPrompt)
		if !compatibilityExists || compatibilityPrompt.Fingerprint != compatibilityDoc.Fingerprint {
			changed = true
		}
		generated = append(generated, compatibilityPrompt)

		timelinePrompt := buildMigrationTimelineDoc(template)
		timelinePrompt.ID = timelineID
		timelinePrompt.Program = program
		timelinePrompt.Type = docTypeMigrationTimeline
		timelinePrompt.SourcePath = template.Source
		if timelinePrompt.SourcePath == "" {
			timelinePrompt.SourcePath = record.SourcePath
		}
		timelinePrompt.Summary = fmt.Sprintf("Migration timeline for %s", program)
		timelinePrompt.Extra = mergeExtra(timelinePrompt.Extra, version)
		timelinePrompt.Fingerprint = kb.ComputeFingerprint(timelinePrompt)
		if !timelineExists || timelinePrompt.Fingerprint != timelineDoc.Fingerprint {
			changed = true
		}
		generated = append(generated, timelinePrompt)
		return nil
	})
	if buildErr != nil {
		return nil, false, buildErr
	}

	if len(generated) == 0 {
		return docs, changed, nil
	}

	sort.SliceStable(generated, func(i, j int) bool {
		if strings.EqualFold(generated[i].Program, generated[j].Program) {
			return generated[i].Type < generated[j].Type
		}
		return strings.ToUpper(generated[i].Program) < strings.ToUpper(generated[j].Program)
	})

	finalDocs := make([]kb.Doc, 0, len(docs))
	for _, doc := range docs {
		if _, managed := managedTypes[strings.ToLower(strings.TrimSpace(doc.Type))]; managed {
			continue
		}
		finalDocs = append(finalDocs, doc)
	}
	finalDocs = append(finalDocs, generated...)
	return finalDocs, changed, nil
}

func documentationDocID(program, kind string) string {
	normalized := strings.ToUpper(strings.TrimSpace(program))
	if normalized == "" {
		normalized = "UNKNOWN"
	}
	return fmt.Sprintf("%s:%s", normalized, kind)
}

func newDocumentationTemplate(record metadata.ProgramRecord, result ctxbuilder.ProgramResult) documentationTemplate {
	tmpl := documentationTemplate{}
	tmpl.Program = strings.TrimSpace(result.Program)
	if tmpl.Program == "" {
		tmpl.Program = strings.TrimSpace(record.Name)
	}
	if tmpl.Program == "" && result.Document != nil {
		tmpl.Program = strings.TrimSpace(result.Document.Program)
	}

	source := strings.TrimSpace(record.SourcePath)
	if source == "" && result.Document != nil {
		source = strings.TrimSpace(result.Document.SourcePath)
	}
	tmpl.Source = source

	summary := strings.TrimSpace(record.Summary)
	if summary == "" && result.Metadata != nil {
		summary = strings.TrimSpace(result.Metadata.Summary)
	}
	tmpl.Summary = summary

	technologies := append([]string(nil), record.Technologies...)
	if result.Document != nil {
		technologies = append(technologies, result.Document.Technologies...)
		tmpl.Inputs = uniqueOrderedStrings(result.Document.Inputs)
		tmpl.Outputs = uniqueOrderedStrings(result.Document.Outputs)
		tmpl.Calls = uniqueOrderedStrings(result.Document.Calls)
	}
	tmpl.Technologies = uniqueOrderedStrings(technologies)

	tmpl.FlowSteps = collectFlowNarrative(result.Document)
	tmpl.Functional = collectFunctionalSpecs(result.Document)
	tmpl.Business = collectBusinessRules(result.Document)
	tmpl.Technical = uniqueLines(collectTechnicalSpecs(record, result))
	tmpl.DataModels = collectDataModelInsights(result.Document)
	tmpl.DTOs = collectDTOInsights(result.Document)
	return tmpl
}

func (t documentationTemplate) renderDoc(title string, instructions []string, sections []templateSection) string {
	trimmedTitle := strings.TrimSpace(title)
	if trimmedTitle == "" {
		trimmedTitle = "Documentation Prompt"
	}
	builder := &strings.Builder{}
	builder.WriteString(trimmedTitle)
	builder.WriteString("\n")

	program := strings.TrimSpace(t.Program)
	if program == "" {
		program = "UNKNOWN"
	}
	builder.WriteString(fmt.Sprintf("Program: %s\n", program))
	if source := strings.TrimSpace(t.Source); source != "" {
		builder.WriteString(fmt.Sprintf("Source: %s\n", source))
	}
	if summary := strings.TrimSpace(t.Summary); summary != "" {
		builder.WriteString("Summary: ")
		builder.WriteString(summary)
		builder.WriteString("\n")
	}
	if len(t.Technologies) > 0 {
		builder.WriteString("Technologies: ")
		builder.WriteString(strings.Join(t.Technologies, ", "))
		builder.WriteString("\n")
	}

	cleanedInstructions := uniqueLines(instructions)
	if len(cleanedInstructions) > 0 {
		builder.WriteString("\nGuidance:\n")
		for _, inst := range cleanedInstructions {
			builder.WriteString("- ")
			builder.WriteString(inst)
			builder.WriteString("\n")
		}
	}

	for _, section := range sections {
		builder.WriteString("\n")
		builder.WriteString(section.Title)
		builder.WriteString(":\n")
		lines := uniqueLines(section.Lines)
		if len(lines) == 0 {
			builder.WriteString("- None\n")
			continue
		}
		for _, line := range lines {
			builder.WriteString("- ")
			builder.WriteString(line)
			builder.WriteString("\n")
		}
	}

	return strings.TrimSpace(builder.String())
}

func formatMetadataVersion(record metadata.ProgramRecord) string {
	if record.LastDocumentUpdate != nil {
		return record.LastDocumentUpdate.UTC().Format(time.RFC3339Nano)
	}
	return record.UpdatedAt.UTC().Format(time.RFC3339Nano)
}

func docMatchesVersion(doc kb.Doc, version string) bool {
	if version == "" {
		return false
	}
	if doc.Extra == nil {
		return false
	}
	if current, ok := doc.Extra[metadataVersionKey]; ok {
		return current == version
	}
	return false
}

func mergeExtra(existing map[string]string, version string) map[string]string {
	if existing == nil {
		existing = make(map[string]string, 1)
	}
	existing[metadataVersionKey] = version
	return existing
}

func buildSummaryDoc(record metadata.ProgramRecord, result ctxbuilder.ProgramResult) kb.Doc {
	doc := kb.Doc{}
	builder := &strings.Builder{}

	summary := strings.TrimSpace(record.Summary)
	if summary == "" && result.Metadata != nil {
		summary = strings.TrimSpace(result.Metadata.Summary)
	}
	if summary != "" {
		builder.WriteString(summary)
		builder.WriteString("\n\n")
	}

	technical := collectTechnicalSpecs(record, result)
	functional := collectFunctionalSpecs(result.Document)
	business := collectBusinessRules(result.Document)

	writeSection(builder, "Technical specifications", technical)
	writeSection(builder, "Functional specifications", functional)
	writeSection(builder, "Business rules", business)

	doc.Content = strings.TrimSpace(builder.String())
	return doc
}

func buildCrossReferenceDoc(ctx context.Context, store metadata.Store, projectID string, record metadata.ProgramRecord, result ctxbuilder.ProgramResult) kb.Doc {
	doc := kb.Doc{}
	builder := &strings.Builder{}

	builder.WriteString("Downstream dependencies:\n")
	writeNeighbors(builder, result.Graph.Dependencies)

	builder.WriteString("\nRelated programs:\n")
	writeNeighbors(builder, result.Graph.Related)

	if store != nil {
		refs, err := store.CrossReferences(ctx, metadata.CrossReferenceOptions{ProjectID: projectID, Target: record.Name, Limit: 25})
		if err == nil && len(refs) > 0 {
			builder.WriteString("\nCatalog references:\n")
			for _, ref := range refs {
				builder.WriteString(fmt.Sprintf("- %s (%s)\n", strings.TrimSpace(ref.Program), strings.TrimSpace(ref.Kind)))
			}
		}
	}

	doc.Content = strings.TrimSpace(builder.String())
	return doc
}

func buildImpactDoc(ctx context.Context, store metadata.Store, projectID string, record metadata.ProgramRecord, result ctxbuilder.ProgramResult) kb.Doc {
	doc := kb.Doc{}
	builder := &strings.Builder{}

	builder.WriteString("Upstream impacts:\n")
	writeNeighbors(builder, result.Graph.Impacts)

	if store != nil {
		counts, err := store.DependencyCounts(ctx, projectID, &record.ID)
		if err == nil && len(counts) > 0 {
			builder.WriteString("\nRelationship counts:\n")
			for _, count := range counts {
				builder.WriteString(fmt.Sprintf("- %s: %d\n", strings.TrimSpace(count.Kind), count.Count))
			}
		}
	}

	if result.Document != nil && len(result.Document.WorkingNotes) > 0 {
		builder.WriteString("\nWorking notes insights:\n")
		for _, note := range result.Document.WorkingNotes {
			if note.Summary != "" {
				builder.WriteString("- ")
				builder.WriteString(strings.TrimSpace(note.Summary))
				builder.WriteString("\n")
			}
		}
	}

	doc.Content = strings.TrimSpace(builder.String())
	return doc
}

func buildProgramFlowDoc(t documentationTemplate) kb.Doc {
	instructions := []string{
		"Describe the control flow from initialization through completion in present tense.",
		"Highlight decision points, loops, and error branches with their conditions.",
		"Connect each step to the inputs, outputs, and external calls that participate.",
	}
	sections := []templateSection{
		{Title: "Primary flow steps", Lines: t.FlowSteps},
		{Title: "Inputs", Lines: t.Inputs},
		{Title: "Outputs", Lines: t.Outputs},
		{Title: "External calls", Lines: t.Calls},
		{Title: "Functional highlights", Lines: t.Functional},
	}
	content := t.renderDoc("Program Flow Prompt Template", instructions, sections)
	return kb.Doc{
		Content:      content,
		Technologies: t.Technologies,
		Inputs:       t.Inputs,
		Outputs:      t.Outputs,
		Calls:        t.Calls,
		Logic:        t.FlowSteps,
	}
}

func buildBusinessRulesDoc(t documentationTemplate) kb.Doc {
	instructions := []string{
		"Enumerate mandatory validations, policy checks, and regulatory constraints.",
		"Group rules by the flow stage or data set they govern.",
		"Call out data quality and exception handling requirements tied to the rules.",
	}
	sections := []templateSection{
		{Title: "Business rules", Lines: t.Business},
		{Title: "Flow checkpoints", Lines: t.FlowSteps},
		{Title: "Data constraints", Lines: t.DataModels},
	}
	content := t.renderDoc("Business Rules Prompt Template", instructions, sections)
	return kb.Doc{
		Content:      content,
		Technologies: t.Technologies,
		Logic:        t.FlowSteps,
	}
}

func buildFunctionalSpecDoc(t documentationTemplate) kb.Doc {
	instructions := []string{
		"Explain how the program fulfils its functional responsibilities for users and batch schedules.",
		"Trace how inputs are transformed into outputs across major flow stages.",
		"Note where business rules or external interactions influence behaviour.",
	}
	sections := []templateSection{
		{Title: "Functional responsibilities", Lines: t.Functional},
		{Title: "Control flow checkpoints", Lines: t.FlowSteps},
		{Title: "Business considerations", Lines: t.Business},
		{Title: "Inputs", Lines: t.Inputs},
		{Title: "Outputs", Lines: t.Outputs},
	}
	content := t.renderDoc("Functional Specification Prompt Template", instructions, sections)
	return kb.Doc{
		Content:      content,
		Technologies: t.Technologies,
		Inputs:       t.Inputs,
		Outputs:      t.Outputs,
		Calls:        t.Calls,
	}
}

func buildTechnicalSpecDoc(t documentationTemplate) kb.Doc {
	instructions := []string{
		"Document platform dependencies, batch constraints, and integration points relevant to modernization.",
		"Summarize data model entities, files, and schemas that must be preserved.",
		"Capture DTO, API, or message contracts that emerge from the current implementation.",
	}
	sections := []templateSection{
		{Title: "Technical specifications", Lines: t.Technical},
		{Title: "Data model considerations", Lines: t.DataModels},
		{Title: "DTO and integration mappings", Lines: t.DTOs},
		{Title: "External calls", Lines: t.Calls},
		{Title: "Inputs", Lines: t.Inputs},
		{Title: "Outputs", Lines: t.Outputs},
	}
	content := t.renderDoc("Technical Specification Prompt Template", instructions, sections)
	return kb.Doc{
		Content:      content,
		Technologies: t.Technologies,
		Inputs:       t.Inputs,
		Outputs:      t.Outputs,
		Calls:        t.Calls,
	}
}

func buildMigrationAssessmentDoc(t documentationTemplate) kb.Doc {
	instructions := []string{
		"Evaluate modernization readiness based on technologies, integrations, and data complexity.",
		"Identify high-risk dependencies or business rules that could delay migration efforts.",
		"Recommend mitigation strategies and prerequisites to reduce migration risk.",
	}
	sections := []templateSection{
		{Title: "Legacy technologies", Lines: t.Technologies},
		{Title: "External dependencies", Lines: t.Calls},
		{Title: "Key data models", Lines: t.DataModels},
		{Title: "Business constraints", Lines: t.Business},
		{Title: "Functional highlights", Lines: t.Functional},
	}
	content := t.renderDoc("Migration Assessment Prompt Template", instructions, sections)
	return kb.Doc{
		Content:      content,
		Technologies: t.Technologies,
		Inputs:       t.Inputs,
		Outputs:      t.Outputs,
		Calls:        t.Calls,
	}
}

func buildComponentMappingDoc(t documentationTemplate) kb.Doc {
	instructions := []string{
		"Map legacy responsibilities to proposed target components or services.",
		"Highlight data structures or DTOs that influence component boundaries.",
		"Call out integration points that require shared contracts across components.",
	}
	sections := []templateSection{
		{Title: "Functional capabilities", Lines: t.Functional},
		{Title: "Control flow checkpoints", Lines: t.FlowSteps},
		{Title: "Data entities and DTOs", Lines: append(append([]string{}, t.DataModels...), t.DTOs...)},
		{Title: "External integrations", Lines: t.Calls},
	}
	content := t.renderDoc("Component Mapping Prompt Template", instructions, sections)
	return kb.Doc{
		Content:      content,
		Technologies: t.Technologies,
		Inputs:       t.Inputs,
		Outputs:      t.Outputs,
		Calls:        t.Calls,
	}
}

func buildAPICompatibilityDoc(t documentationTemplate) kb.Doc {
	instructions := []string{
		"Compare legacy inputs and outputs with the target API expectations.",
		"List DTOs or message schemas that require translation or remain compatible.",
		"Document breaking changes, required adapters, and validation rules affecting consumers.",
	}
	sections := []templateSection{
		{Title: "Legacy inputs", Lines: t.Inputs},
		{Title: "Legacy outputs", Lines: t.Outputs},
		{Title: "DTO and schema inventory", Lines: t.DTOs},
		{Title: "External API calls", Lines: t.Calls},
		{Title: "Business and validation rules", Lines: t.Business},
	}
	content := t.renderDoc("API Compatibility Matrix Prompt Template", instructions, sections)
	return kb.Doc{
		Content:      content,
		Technologies: t.Technologies,
		Inputs:       t.Inputs,
		Outputs:      t.Outputs,
		Calls:        t.Calls,
	}
}

func buildMigrationTimelineDoc(t documentationTemplate) kb.Doc {
	instructions := []string{
		"Outline migration phases from assessment through cutover and validation.",
		"Note dependencies, prerequisites, and parallel workstreams for each phase.",
		"Call out business events or functional checkpoints that influence scheduling.",
	}
	sections := []templateSection{
		{Title: "Key flow milestones", Lines: t.FlowSteps},
		{Title: "Technical prerequisites", Lines: t.Technical},
		{Title: "Business readiness factors", Lines: t.Business},
		{Title: "Integration checkpoints", Lines: t.Calls},
	}
	content := t.renderDoc("Migration Timeline Prompt Template", instructions, sections)
	return kb.Doc{
		Content:      content,
		Technologies: t.Technologies,
		Inputs:       t.Inputs,
		Outputs:      t.Outputs,
		Calls:        t.Calls,
	}
}

func writeNeighbors(builder *strings.Builder, neighbors []kb.GraphNeighbor) {
	if len(neighbors) == 0 {
		builder.WriteString("- None\n")
		return
	}
	for _, neighbor := range neighbors {
		builder.WriteString("- ")
		builder.WriteString(strings.TrimSpace(neighbor.Program))
		if neighbor.Name != "" {
			builder.WriteString(" (" + strings.TrimSpace(neighbor.Name) + ")")
		}
		if len(neighbor.Chain) > 0 {
			builder.WriteString(" via ")
			builder.WriteString(strings.Join(neighbor.Chain, " -> "))
		}
		if neighbor.Distance > 0 {
			builder.WriteString(fmt.Sprintf(" [distance %d]", neighbor.Distance))
		}
		builder.WriteString("\n")
	}
}

func collectTechnicalSpecs(record metadata.ProgramRecord, result ctxbuilder.ProgramResult) []string {
	specs := make([]string, 0, 12)

	source := strings.TrimSpace(record.SourcePath)
	if source == "" && result.Document != nil {
		source = strings.TrimSpace(result.Document.SourcePath)
	}
	if source != "" {
		specs = append(specs, fmt.Sprintf("Source file: %s", source))
	}

	technologies := make([]string, 0, len(record.Technologies))
	technologies = append(technologies, record.Technologies...)
	if result.Document != nil {
		technologies = append(technologies, result.Document.Technologies...)
	}
	if techs := uniqueOrderedStrings(technologies); len(techs) > 0 {
		specs = append(specs, fmt.Sprintf("Technologies: %s", strings.Join(techs, ", ")))
	}

	if record.DocumentCount > 0 || record.DocTypeCount > 0 {
		specs = append(specs, fmt.Sprintf("Documentation coverage: %d documents across %d types", record.DocumentCount, record.DocTypeCount))
	}

	if result.Document != nil {
		specs = appendSpecList(specs, "Inputs", result.Document.Inputs)
		specs = appendSpecList(specs, "Outputs", result.Document.Outputs)
		specs = appendSpecList(specs, "External calls", result.Document.Calls)
		specs = appendSpecList(specs, "Paragraphs", result.Document.Paragraphs)
		if len(result.Document.Logic) > 0 {
			specs = append(specs, fmt.Sprintf("Logic highlights: %s", strings.Join(uniqueOrderedStrings(result.Document.Logic), "; ")))
		}
	}

	if result.Graph.Program != "" {
		specs = append(specs, fmt.Sprintf("Graph coverage: %d dependencies, %d impacts, %d related programs",
			len(result.Graph.Dependencies), len(result.Graph.Impacts), len(result.Graph.Related)))
	}

	return specs
}

func collectFlowNarrative(doc *kb.ProgramDoc) []string {
	if doc == nil {
		return nil
	}
	var lines []string
	lines = append(lines, doc.Logic...)
	for _, flow := range doc.Flows {
		lines = append(lines, extractDocLines(flow)...)
	}
	return uniqueLines(lines)
}

func collectFunctionalSpecs(doc *kb.ProgramDoc) []string {
	if doc == nil {
		return nil
	}
	var lines []string
	lines = append(lines, collectFlowNarrative(doc)...)
	for _, note := range doc.WorkingNotes {
		lines = append(lines, extractDocLines(note)...)
	}
	return uniqueLines(lines)
}

func collectBusinessRules(doc *kb.ProgramDoc) []string {
	if doc == nil {
		return nil
	}
	var lines []string
	for _, rule := range doc.BusinessRules {
		lines = append(lines, extractDocLines(rule)...)
	}
	return uniqueLines(lines)
}

func collectDataModelInsights(doc *kb.ProgramDoc) []string {
	if doc == nil {
		return nil
	}
	var lines []string
	for _, note := range doc.WorkingNotes {
		lines = append(lines, extractDocLines(note)...)
	}
	for _, rule := range doc.BusinessRules {
		lines = append(lines, extractDocLines(rule)...)
	}
	for _, modern := range doc.Modernization {
		lines = append(lines, extractDocLines(modern)...)
	}
	return filterLines(lines, []string{"table", "column", "record", "schema", "segment", "field", "layout", "db2", "ims", "database", "file", "copybook"})
}

func collectDTOInsights(doc *kb.ProgramDoc) []string {
	if doc == nil {
		return nil
	}
	var lines []string
	for _, mapping := range doc.ConversionMappings {
		lines = append(lines, extractDocLines(mapping)...)
	}
	for _, summary := range doc.ConversionSummaries {
		lines = append(lines, extractDocLines(summary)...)
	}
	for _, source := range doc.ConversionSources {
		lines = append(lines, extractDocLines(source)...)
	}
	for _, modern := range doc.Modernization {
		lines = append(lines, extractDocLines(modern)...)
	}
	return filterLines(lines, []string{"dto", "entity", "class", "mapping", "request", "response", "json", "api", "service", "payload", "object"})
}

func writeSection(builder *strings.Builder, title string, lines []string) {
	cleaned := uniqueLines(lines)
	if len(cleaned) == 0 {
		return
	}
	if builder.Len() > 0 {
		builder.WriteString("\n")
	}
	builder.WriteString(title)
	builder.WriteString(":\n")
	for _, line := range cleaned {
		builder.WriteString("- ")
		builder.WriteString(line)
		builder.WriteString("\n")
	}
}

func extractDocLines(doc kb.Doc) []string {
	text := strings.TrimSpace(doc.Content)
	if text == "" {
		text = strings.TrimSpace(doc.Summary)
	}
	if text == "" {
		return nil
	}
	raw := strings.Split(text, "\n")
	lines := make([]string, 0, len(raw))
	for _, line := range raw {
		normalized := normalizeSpecLine(line)
		if normalized == "" {
			continue
		}
		lower := strings.ToLower(normalized)
		if strings.HasPrefix(lower, "business rules inferred") {
			continue
		}
		lines = append(lines, normalized)
	}
	return lines
}

func filterLines(lines []string, keywords []string) []string {
	if len(lines) == 0 || len(keywords) == 0 {
		return nil
	}
	filtered := make([]string, 0, len(lines))
	for _, line := range lines {
		trimmed := strings.TrimSpace(line)
		if trimmed == "" {
			continue
		}
		lower := strings.ToLower(trimmed)
		for _, keyword := range keywords {
			if strings.Contains(lower, keyword) {
				filtered = append(filtered, trimmed)
				break
			}
		}
	}
	return uniqueLines(filtered)
}

func normalizeSpecLine(line string) string {
	trimmed := strings.TrimSpace(line)
	for _, prefix := range []string{"- ", "* ", "• ", "– ", "— "} {
		if strings.HasPrefix(trimmed, prefix) {
			trimmed = strings.TrimSpace(trimmed[len(prefix):])
		}
	}
	for len(trimmed) > 0 {
		r, size := utf8.DecodeRuneInString(trimmed)
		if r != '-' && r != '*' && r != '•' && r != '–' && r != '—' {
			break
		}
		trimmed = strings.TrimSpace(trimmed[size:])
	}
	return strings.TrimSpace(trimmed)
}

func uniqueLines(lines []string) []string {
	if len(lines) == 0 {
		return nil
	}
	seen := make(map[string]struct{}, len(lines))
	ordered := make([]string, 0, len(lines))
	for _, line := range lines {
		trimmed := strings.TrimSpace(line)
		if trimmed == "" {
			continue
		}
		key := strings.ToLower(trimmed)
		if _, ok := seen[key]; ok {
			continue
		}
		seen[key] = struct{}{}
		ordered = append(ordered, trimmed)
	}
	return ordered
}

func appendSpecList(specs []string, label string, values []string) []string {
	if cleaned := uniqueOrderedStrings(values); len(cleaned) > 0 {
		specs = append(specs, fmt.Sprintf("%s: %s", label, strings.Join(cleaned, ", ")))
	}
	return specs
}

func uniqueOrderedStrings(values []string) []string {
	if len(values) == 0 {
		return nil
	}
	seen := make(map[string]struct{}, len(values))
	ordered := make([]string, 0, len(values))
	for _, value := range values {
		trimmed := strings.TrimSpace(value)
		if trimmed == "" {
			continue
		}
		key := strings.ToLower(trimmed)
		if _, ok := seen[key]; ok {
			continue
		}
		seen[key] = struct{}{}
		ordered = append(ordered, trimmed)
	}
	return ordered
}
