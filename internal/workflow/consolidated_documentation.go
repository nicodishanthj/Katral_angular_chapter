// File path: internal/workflow/consolidated_documentation.go
package workflow

import (
	"archive/zip"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"github.com/nicodishanthj/Katral_phase1/internal/kb"
)

// ConsolidatedDoc captures a project-wide documentation snapshot that aggregates
// all generated artifacts into a single manifest for downstream consumption.
type ConsolidatedDoc struct {
	ProjectID     string                     `json:"project_id"`
	GeneratedAt   time.Time                  `json:"generated_at"`
	DocumentCount int                        `json:"document_count"`
	TypeCounts    map[string]int             `json:"type_counts,omitempty"`
	Programs      []ConsolidatedProgramDocs  `json:"programs"`
	Documents     []ConsolidatedDocumentInfo `json:"documents"`
}

// ConsolidatedProgramDocs groups documentation artifacts by their source
// program to simplify rendering logic for clients that consume the manifest.
type ConsolidatedProgramDocs struct {
	Program   string                     `json:"program"`
	Documents []ConsolidatedDocumentInfo `json:"documents"`
}

// ConsolidatedDocumentInfo describes an individual documentation artifact that
// was consolidated into the manifest.
type ConsolidatedDocumentInfo struct {
	ID       string `json:"id,omitempty"`
	Type     string `json:"type,omitempty"`
	Title    string `json:"title,omitempty"`
	Program  string `json:"program,omitempty"`
	Summary  string `json:"summary,omitempty"`
	Source   string `json:"source,omitempty"`
	Markdown string `json:"markdown,omitempty"`
}

func (m *Manager) generateConsolidatedDocumentation(ctx context.Context, projectID string, docs []kb.Doc) (string, error) {
	projectID = strings.TrimSpace(projectID)
	if projectID == "" || len(docs) == 0 {
		return "", nil
	}
	select {
	case <-ctx.Done():
		return "", ctx.Err()
	default:
	}
	consolidated := buildConsolidatedDoc(projectID, docs)
	if consolidated.DocumentCount == 0 {
		return "", nil
	}
	root, err := m.ensureArtifactRoot()
	if err != nil {
		return "", err
	}
	timestamp := time.Now().UTC().Format("20060102T150405Z")
	safeProject := safeFileComponent(projectID)
	artifactName := fmt.Sprintf("%s-consolidated-%s.zip", safeProject, timestamp)
	finalPath := filepath.Join(root, artifactName)
	tempPath := finalPath + ".tmp"

	file, err := os.OpenFile(tempPath, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0o644)
	if err != nil {
		return "", fmt.Errorf("create consolidated artifact: %w", err)
	}
	zipWriter := zip.NewWriter(file)
	cleanup := func() {
		_ = zipWriter.Close()
		_ = file.Close()
		_ = os.Remove(tempPath)
	}
	manifestPayload, err := json.MarshalIndent(consolidated, "", "  ")
	if err != nil {
		cleanup()
		return "", fmt.Errorf("marshal consolidated manifest: %w", err)
	}
	manifestWriter, err := zipWriter.Create("manifest.json")
	if err != nil {
		cleanup()
		return "", fmt.Errorf("create consolidated manifest: %w", err)
	}
	if _, err := manifestWriter.Write(manifestPayload); err != nil {
		cleanup()
		return "", fmt.Errorf("write consolidated manifest: %w", err)
	}
	markdown := renderConsolidatedMarkdown(consolidated)
	if strings.TrimSpace(markdown) != "" {
		docWriter, err := zipWriter.Create("documentation.md")
		if err != nil {
			cleanup()
			return "", fmt.Errorf("create consolidated markdown: %w", err)
		}
		if _, err := docWriter.Write([]byte(markdown)); err != nil {
			cleanup()
			return "", fmt.Errorf("write consolidated markdown: %w", err)
		}
	}
	if err := zipWriter.Close(); err != nil {
		cleanup()
		return "", fmt.Errorf("finalize consolidated artifact: %w", err)
	}
	if err := file.Close(); err != nil {
		cleanup()
		return "", fmt.Errorf("close consolidated artifact: %w", err)
	}
	if err := os.Rename(tempPath, finalPath); err != nil {
		_ = os.Remove(tempPath)
		return "", fmt.Errorf("finalize consolidated artifact: %w", err)
	}
	absPath, err := filepath.Abs(finalPath)
	if err != nil {
		_ = os.Remove(finalPath)
		return "", fmt.Errorf("resolve consolidated artifact: %w", err)
	}
	return absPath, nil
}

func buildConsolidatedDoc(projectID string, docs []kb.Doc) ConsolidatedDoc {
	consolidated := ConsolidatedDoc{
		ProjectID:   projectID,
		GeneratedAt: time.Now().UTC(),
	}
	if len(docs) == 0 {
		return consolidated
	}
	programs := make(map[string][]ConsolidatedDocumentInfo)
	typeCounts := make(map[string]int)
	flatDocuments := make([]ConsolidatedDocumentInfo, 0, len(docs))
	for _, doc := range docs {
		info := makeConsolidatedDocumentInfo(doc)
		if info.Program == "" {
			info.Program = "Project"
		}
		consolidated.DocumentCount++
		if info.Type != "" {
			typeKey := strings.ToLower(info.Type)
			typeCounts[typeKey]++
		}
		programs[info.Program] = append(programs[info.Program], info)
		flatDocuments = append(flatDocuments, info)
	}
	if len(typeCounts) > 0 {
		consolidated.TypeCounts = typeCounts
	}
	consolidated.Documents = sortConsolidatedDocuments(flatDocuments)
	consolidated.Programs = sortConsolidatedPrograms(programs)
	return consolidated
}

func makeConsolidatedDocumentInfo(doc kb.Doc) ConsolidatedDocumentInfo {
	info := ConsolidatedDocumentInfo{
		ID:      strings.TrimSpace(doc.ID),
		Type:    strings.TrimSpace(doc.Type),
		Program: strings.TrimSpace(doc.Program),
		Summary: strings.TrimSpace(doc.Summary),
		Source:  strings.TrimSpace(doc.SourcePath),
	}
	info.Markdown = strings.TrimSpace(documentationMarkdownFromDoc(doc))
	info.Title = consolidatedTitleForDoc(doc, info)
	return info
}

func sortConsolidatedDocuments(docs []ConsolidatedDocumentInfo) []ConsolidatedDocumentInfo {
	if len(docs) == 0 {
		return nil
	}
	sorted := append([]ConsolidatedDocumentInfo(nil), docs...)
	sort.Slice(sorted, func(i, j int) bool {
		left := sorted[i]
		right := sorted[j]
		if !strings.EqualFold(left.Program, right.Program) {
			return strings.ToUpper(left.Program) < strings.ToUpper(right.Program)
		}
		if !strings.EqualFold(left.Type, right.Type) {
			return strings.ToUpper(left.Type) < strings.ToUpper(right.Type)
		}
		return left.ID < right.ID
	})
	return sorted
}

func sortConsolidatedPrograms(programs map[string][]ConsolidatedDocumentInfo) []ConsolidatedProgramDocs {
	if len(programs) == 0 {
		return nil
	}
	names := make([]string, 0, len(programs))
	for name := range programs {
		names = append(names, name)
	}
	sort.Slice(names, func(i, j int) bool {
		return strings.ToUpper(names[i]) < strings.ToUpper(names[j])
	})
	sections := make([]ConsolidatedProgramDocs, 0, len(names))
	for _, name := range names {
		docs := sortConsolidatedDocuments(programs[name])
		sections = append(sections, ConsolidatedProgramDocs{Program: name, Documents: docs})
	}
	return sections
}

func consolidatedTitleForDoc(doc kb.Doc, info ConsolidatedDocumentInfo) string {
	switch strings.ToLower(strings.TrimSpace(doc.Type)) {
	case docTypeSummary:
		return "Program Summary"
	case docTypeCrossReference:
		return "Cross-Reference Map"
	case docTypeImpact:
		return "Impact Assessment"
	case docTypeProgramFlow:
		return "Program Flow Guidance"
	case docTypeBusinessPrompt:
		return "Business Rules Guidance"
	case docTypeFunctionalSpec:
		return "Functional Specification"
	case docTypeTechnicalSpec:
		return "Technical Specification"
	case docTypeMigrationAssessment:
		return "Migration Assessment Report"
	case docTypeComponentMapping:
		return "Component Mapping Guide"
	case docTypeAPICompatibility:
		return "API Compatibility Matrix"
	case docTypeMigrationTimeline:
		return "Migration Timeline"
	case "metadata":
		return "Program Metadata"
	case "flow", "cics_flow", "mq_flow":
		return "Program Flow"
	case "business_rules":
		return "Business Rules"
	case "working_notes":
		return "Working Notes"
	case "modernization":
		return "Modernization Blueprint"
	default:
		if info.Summary != "" {
			return info.Summary
		}
		if info.ID != "" {
			return info.ID
		}
		return "Documentation"
	}
}

func renderConsolidatedMarkdown(doc ConsolidatedDoc) string {
	if doc.DocumentCount == 0 {
		return ""
	}
	var builder strings.Builder
	builder.WriteString("# Project Documentation Summary\n\n")
	if doc.ProjectID != "" {
		builder.WriteString(fmt.Sprintf("Project: %s\n\n", doc.ProjectID))
	}
	builder.WriteString(fmt.Sprintf("Generated: %s\n\n", doc.GeneratedAt.Format(time.RFC3339)))
	for _, program := range doc.Programs {
		if program.Program != "" {
			builder.WriteString(fmt.Sprintf("## %s\n\n", program.Program))
		}
		for _, entry := range program.Documents {
			title := entry.Title
			if title == "" {
				title = entry.Type
			}
			if title == "" {
				title = entry.ID
			}
			if title != "" {
				builder.WriteString(fmt.Sprintf("### %s\n\n", title))
			}
			if entry.Type != "" {
				builder.WriteString(fmt.Sprintf("_Type_: %s\n\n", entry.Type))
			}
			if entry.Source != "" {
				builder.WriteString(fmt.Sprintf("_Source_: %s\n\n", entry.Source))
			}
			if entry.Summary != "" && entry.Summary != title {
				builder.WriteString(fmt.Sprintf("_Summary_: %s\n\n", entry.Summary))
			}
			if entry.Markdown != "" {
				builder.WriteString(entry.Markdown)
				if !strings.HasSuffix(entry.Markdown, "\n") {
					builder.WriteString("\n")
				}
				builder.WriteString("\n")
			}
		}
	}
	return strings.TrimSpace(builder.String()) + "\n"
}
