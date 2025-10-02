// File path: internal/kb/cobol_analyzer.go
package kb

import (
	"context"
	"fmt"
	"strings"

	"github.com/nicodishanthj/Katral_phase1/internal/kb/cobol"
)

type cobolAnalyzer struct {
	parser *cobol.Parser
}

func (c *cobolAnalyzer) Name() string { return "cobol" }

func (c *cobolAnalyzer) Match(path string, data []byte) bool {
	lower := strings.ToLower(path)
	if strings.HasSuffix(lower, ".cob") || strings.HasSuffix(lower, ".cbl") {
		return true
	}
	content := strings.ToUpper(string(data))
	return strings.Contains(content, "IDENTIFICATION DIVISION") || strings.Contains(content, "PROCEDURE DIVISION")
}

func (c *cobolAnalyzer) Parse(ctx context.Context, path string, data []byte) ([]Doc, error) {
	if c.parser == nil {
		return nil, fmt.Errorf("nil COBOL parser")
	}
	docs, err := c.parser.Parse(ctx, path, data)
	if err != nil {
		return nil, err
	}
	var out []Doc
	for _, doc := range docs {
		converted := Doc{
			ID:           doc.ID,
			Program:      doc.Program,
			SourcePath:   doc.SourcePath,
			Chunk:        doc.Chunk,
			Type:         doc.Type,
			Content:      doc.Content,
			Summary:      doc.Summary,
			Technologies: []string{"COBOL"},
			Inputs:       doc.Inputs,
			Outputs:      doc.Outputs,
			Calls:        doc.Calls,
			Paragraphs:   doc.Paragraphs,
			Logic:        doc.Logic,
			LogicTree:    doc.LogicTree,
			Extra:        doc.Extra,
		}

		out = append(out, converted)
		if doc.Type == "metadata" {
			flowContent, flowDiagram := buildFlowContent(doc.Program, doc.Inputs, doc.Outputs, doc.Calls, doc.LogicTree)
			if flowContent != "" || flowDiagram != nil {
				out = append(out, Doc{
					ID:           fmt.Sprintf("%s:flow", doc.Program),
					Program:      doc.Program,
					SourcePath:   doc.SourcePath,
					Chunk:        -2,
					Type:         "flow",
					Content:      flowContent,
					Summary:      fmt.Sprintf("Flow documentation for %s", doc.Program),
					Technologies: []string{"COBOL"},
					Inputs:       doc.Inputs,
					Outputs:      doc.Outputs,
					Calls:        doc.Calls,
					FlowDiagram:  flowDiagram,
				})
				workingContent := buildWorkingContent(doc.Program, doc.Paragraphs, doc.Extra)
				if workingContent != "" {
					out = append(out, Doc{
						ID:           fmt.Sprintf("%s:working", doc.Program),
						Program:      doc.Program,
						SourcePath:   doc.SourcePath,
						Chunk:        -3,
						Type:         "working_notes",
						Content:      workingContent,
						Summary:      fmt.Sprintf("Working summary for %s", doc.Program),
						Technologies: []string{"COBOL"},
						Paragraphs:   doc.Paragraphs,
						Extra:        doc.Extra,
					})
				}
				businessContent := buildBusinessRules(doc.Program, doc.LogicTree)
				if businessContent != "" {
					out = append(out, Doc{
						ID:           fmt.Sprintf("%s:business", doc.Program),
						Program:      doc.Program,
						SourcePath:   doc.SourcePath,
						Chunk:        -4,
						Type:         "business_rules",
						Content:      businessContent,
						Summary:      fmt.Sprintf("Business rules for %s", doc.Program),
						Technologies: []string{"COBOL"},
						Logic:        doc.Logic,
						LogicTree:    doc.LogicTree,
					})
				}
			}
		}
	}
	return out, nil
}

func buildWorkingContent(program string, paragraphs []string, extra map[string]string) string {
	var sections []string
	sections = append(sections, fmt.Sprintf("Working details for %s", program))
	if divisions := extra["divisions"]; divisions != "" {
		sections = append(sections, fmt.Sprintf("Divisions: %s", divisions))
	}
	if len(paragraphs) > 0 {
		sections = append(sections, "Key Paragraphs:")
		for _, p := range paragraphs {
			sections = append(sections, fmt.Sprintf("- %s", p))
		}
	}
	return strings.Join(sections, "\n")
}
