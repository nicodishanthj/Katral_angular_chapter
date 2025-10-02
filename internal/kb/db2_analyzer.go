// File path: internal/kb/db2_analyzer.go
package kb

import (
	"bufio"
	"context"
	"fmt"
	"regexp"
	"strings"
)

type db2Analyzer struct{}

func (d *db2Analyzer) Name() string { return "db2" }

func (d *db2Analyzer) Match(path string, data []byte) bool {
	content := strings.ToUpper(string(data))
	if strings.Contains(content, "CREATE TABLE") || strings.Contains(content, "CREATE DATABASE") {
		return true
	}
	lower := strings.ToLower(path)
	return strings.HasSuffix(lower, ".ddl") || strings.HasSuffix(lower, ".sql")
}

var (
	db2TableRe  = regexp.MustCompile(`(?is)CREATE\s+TABLE\s+([A-Z0-9_.-]+)\s*\((.*?)\)\s*;`)
	db2ColumnRe = regexp.MustCompile(`(?im)^\s*([A-Z0-9_.-]+)\s+([^,]+)`) // naive column definition
)

type db2Column struct {
	Name        string
	Definition  string
	Constraints []string
}

func extractDB2Columns(block string) []db2Column {
	scanner := bufio.NewScanner(strings.NewReader(block))
	var columns []db2Column
	var current *db2Column
	appendCurrent := func() {
		if current == nil {
			return
		}
		trimmed := strings.TrimSuffix(strings.TrimSpace(current.Definition), ",")
		current.Definition = strings.TrimSpace(trimmed)
		defUpper := strings.ToUpper(current.Definition)
		if strings.Contains(defUpper, "NOT NULL") {
			current.Constraints = append(current.Constraints, fmt.Sprintf("%s must be present", current.Name))
		}
		if strings.Contains(defUpper, "PRIMARY KEY") {
			current.Constraints = append(current.Constraints, fmt.Sprintf("%s is the primary key", current.Name))
		}
		if strings.Contains(defUpper, "UNIQUE") {
			current.Constraints = append(current.Constraints, fmt.Sprintf("%s must remain unique", current.Name))
		}
		if strings.Contains(defUpper, "FOREIGN KEY") {
			current.Constraints = append(current.Constraints, fmt.Sprintf("%s enforces referential integrity", current.Name))
		}
		if strings.Contains(defUpper, "CHECK") {
			current.Constraints = append(current.Constraints, fmt.Sprintf("%s constrained by CHECK", current.Name))
		}
		columns = append(columns, *current)
		current = nil
	}
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" || strings.HasPrefix(line, "--") {
			continue
		}
		if strings.HasPrefix(line, ")") {
			break
		}
		if match := db2ColumnRe.FindStringSubmatch(line); len(match) > 2 {
			if current != nil {
				appendCurrent()
			}
			name := strings.ToUpper(strings.TrimSpace(match[1]))
			definition := strings.TrimSpace(match[2])
			current = &db2Column{Name: name, Definition: definition}
			if strings.HasSuffix(line, ",") {
				appendCurrent()
			}
			continue
		}
		if current != nil {
			segment := strings.TrimSpace(line)
			if strings.HasSuffix(segment, ",") {
				segment = strings.TrimSuffix(segment, ",")
				if segment != "" {
					current.Definition += " " + segment
				}
				appendCurrent()
			} else if segment != "" {
				current.Definition += " " + segment
			}
		}
	}
	if current != nil {
		appendCurrent()
	}
	return columns
}

func (d *db2Analyzer) Parse(ctx context.Context, path string, data []byte) ([]Doc, error) {
	text := string(data)
	matches := db2TableRe.FindAllStringSubmatch(text, -1)
	if len(matches) == 0 {
		return nil, nil
	}
	var docs []Doc
	technologies := []string{"DB2", "SQL"}
	for idx, match := range matches {
		if len(match) < 3 {
			continue
		}
		tableName := strings.ToUpper(strings.TrimSpace(match[1]))
		if tableName == "" {
			tableName = fmt.Sprintf("TABLE_%d", idx)
		}
		columns := extractDB2Columns(match[2])
		if len(columns) == 0 {
			continue
		}
		var columnLines []string
		var columnNames []string
		var constraints []string
		for _, col := range columns {
			columnNames = append(columnNames, col.Name)
			columnLines = append(columnLines, fmt.Sprintf("%s %s", col.Name, col.Definition))
			constraints = append(constraints, col.Constraints...)
		}

		baseDoc := Doc{
			ID:           fmt.Sprintf("%s:db2:%d", tableName, idx),
			Program:      tableName,
			SourcePath:   path,
			Chunk:        -20 - idx*4,
			Type:         "db2_schema",
			Content:      fmt.Sprintf("DB2 table %s\nColumns:\n%s", tableName, strings.Join(prependDash(columnLines), "\n")),
			Summary:      fmt.Sprintf("DB2 table %s with %d columns", tableName, len(columns)),
			Technologies: technologies,
			Extra: map[string]string{
				"columns": strings.Join(columnNames, ", "),
			},
		}

		flowLines := []string{fmt.Sprintf("DB2 flow for %s", tableName), "Column order:"}
		flowLines = append(flowLines, prependDash(columnNames)...)

		workingLines := []string{fmt.Sprintf("Working notes for %s", tableName), "Column definitions:"}
		workingLines = append(workingLines, prependDash(columnLines)...)

		businessLines := []string{fmt.Sprintf("Business rules for %s", tableName)}
		if len(constraints) > 0 {
			businessLines = append(businessLines, prependDash(uniqueStringsFromSlice(constraints))...)
		}

		docs = append(docs, baseDoc)
		if flow := strings.Join(nonEmpty(flowLines), "\n"); flow != "" {
			docs = append(docs, Doc{
				ID:           fmt.Sprintf("%s:db2:flow:%d", tableName, idx),
				Program:      tableName,
				SourcePath:   path,
				Chunk:        -21 - idx*4,
				Type:         "flow",
				Content:      flow,
				Summary:      fmt.Sprintf("DB2 flow for %s", tableName),
				Technologies: technologies,
			})
		}
		if working := strings.Join(nonEmpty(workingLines), "\n"); working != "" {
			docs = append(docs, Doc{
				ID:           fmt.Sprintf("%s:db2:working:%d", tableName, idx),
				Program:      tableName,
				SourcePath:   path,
				Chunk:        -22 - idx*4,
				Type:         "working_notes",
				Content:      working,
				Summary:      fmt.Sprintf("Working notes for %s", tableName),
				Technologies: technologies,
			})
		}
		if business := strings.Join(nonEmpty(businessLines), "\n"); business != "" {
			docs = append(docs, Doc{
				ID:           fmt.Sprintf("%s:db2:business:%d", tableName, idx),
				Program:      tableName,
				SourcePath:   path,
				Chunk:        -23 - idx*4,
				Type:         "business_rules",
				Content:      business,
				Summary:      fmt.Sprintf("Business rules for %s", tableName),
				Technologies: technologies,
			})
		}
	}
	return docs, nil
}
