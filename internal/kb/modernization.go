// File path: internal/kb/modernization.go
package kb

import (
	"fmt"
	"strings"
)

func newModernizationInsights() *modernizationInsights {
	return &modernizationInsights{}
}

type modernizationInsights struct {
	flows         []string
	businessRules []string
	db2Tables     []string
	imsSegments   []string

	cicsOps       []string
	mqOps         []string
	asmModules    []string
	sqlStatements []string
}

func (m *modernizationInsights) Observe(doc Doc) {
	switch doc.Type {
	case "flow":
		if doc.Content != "" {
			m.flows = append(m.flows, doc.Content)
		}
	case "business_rules":
		if doc.Content != "" {
			m.businessRules = append(m.businessRules, doc.Content)
		}
	case "db2_schema":
		if doc.Program != "" {
			m.db2Tables = append(m.db2Tables, doc.Program)
		}
	case "ims_schema":
		if doc.Extra != nil {
			if segs := doc.Extra["segments"]; segs != "" {
				m.imsSegments = append(m.imsSegments, fmt.Sprintf("%s: %s", doc.Program, segs))
			}
		}
	case "cics_flow":
		if doc.Extra != nil {
			if cmds := doc.Extra["commands"]; cmds != "" {
				m.cicsOps = append(m.cicsOps, cmds)
			}
		}
	case "mq_flow":
		if doc.Content != "" {
			m.mqOps = append(m.mqOps, doc.Content)
		}
	case "asm_module":
		if doc.Content != "" {
			m.asmModules = append(m.asmModules, doc.Content)
		}
	case "sql_statement":
		if doc.Content != "" {
			m.sqlStatements = append(m.sqlStatements, doc.Content)
		}
	}
}

func buildModernizationDocs(stacks []string, insights *modernizationInsights) []Doc {
	if insights == nil {
		return nil
	}
	var docs []Doc
	if hasStack(stacks, "springboot") {
		docs = append(docs, buildSpringDoc("springboot", insights))
	}
	if hasStack(stacks, "springbatch") {
		docs = append(docs, buildSpringDoc("springbatch", insights))
	}
	return docs
}

func hasStack(stacks []string, name string) bool {
	for _, stack := range stacks {
		if strings.EqualFold(strings.TrimSpace(stack), name) {
			return true
		}
	}
	return false
}

func buildSpringDoc(target string, insights *modernizationInsights) Doc {
	var lines []string
	title := strings.ToUpper(target) + " modernization outline"
	lines = append(lines, title)
	if len(insights.flows) > 0 {
		lines = append(lines, "Application flows:")
		for _, flow := range insights.flows {
			lines = append(lines, "- "+flow)
		}
	}
	if len(insights.db2Tables) > 0 {
		lines = append(lines, "DB2 schema considerations:")
		for _, table := range uniqueStringsFromSlice(insights.db2Tables) {
			lines = append(lines, fmt.Sprintf("- Map table %s to JPA entity", table))
		}
	}
	if len(insights.imsSegments) > 0 {
		lines = append(lines, "IMS migration notes:")
		for _, seg := range insights.imsSegments {
			lines = append(lines, "- "+seg)
		}
	}

	if len(insights.cicsOps) > 0 {
		lines = append(lines, "CICS interactions:")
		for _, cmd := range uniqueStringsFromSlice(insights.cicsOps) {
			lines = append(lines, fmt.Sprintf("- Replace CICS command sequence %s with REST or messaging", cmd))
		}
	}
	if len(insights.mqOps) > 0 {
		lines = append(lines, "MQ messaging:")
		for _, msg := range insights.mqOps {
			lines = append(lines, "- "+msg)
		}
	}
	if len(insights.businessRules) > 0 {
		lines = append(lines, "Business rules to encapsulate:")
		for _, rule := range insights.businessRules {
			lines = append(lines, "- "+rule)
		}
	}
	if len(insights.sqlStatements) > 0 {
		lines = append(lines, "SQL operations:")
		for _, stmt := range insights.sqlStatements {
			lines = append(lines, "- "+stmt)
		}
	}
	if len(insights.asmModules) > 0 {
		lines = append(lines, "Assembler modules:")
		for _, mod := range insights.asmModules {
			lines = append(lines, "- "+mod)
		}
	}
	summary := fmt.Sprintf("Modernization blueprint targeting %s", target)
	id := fmt.Sprintf("modernization:%s", target)

	return Doc{
		ID:         id,
		Program:    strings.ToUpper(target),
		SourcePath: "modernization",
		Chunk:      -90,
		Type:       "modernization",
		Content:    strings.Join(lines, "\n"),
		Summary:    summary,
		Extra: map[string]string{
			"target": target,
		},
	}
}
