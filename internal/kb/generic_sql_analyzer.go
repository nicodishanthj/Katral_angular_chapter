// File path: internal/kb/generic_sql_analyzer.go
package kb

import (
	"context"
	"fmt"
	"path/filepath"
	"strings"
)

type genericSQLAnalyzer struct{}

func (s *genericSQLAnalyzer) Name() string { return "sql" }

func (s *genericSQLAnalyzer) Match(path string, data []byte) bool {
	lower := strings.ToLower(path)
	if strings.HasSuffix(lower, ".sql") {
		return true
	}
	content := strings.ToUpper(string(data))
	keywords := []string{"SELECT", "INSERT", "UPDATE", "DELETE", "EXEC SQL"}
	for _, kw := range keywords {
		if strings.Contains(content, kw) {
			return true
		}
	}
	return false
}

func (s *genericSQLAnalyzer) Parse(ctx context.Context, path string, data []byte) ([]Doc, error) {
	statements := splitSQLStatements(string(data))
	if len(statements) == 0 {
		return nil, nil
	}
	var docs []Doc
	for idx, stmt := range statements {
		trimmed := strings.TrimSpace(stmt)
		if trimmed == "" {
			continue
		}
		headline := trimmed
		if len(headline) > 120 {
			headline = headline[:120] + "..."
		}
		docs = append(docs, Doc{
			ID:           fmt.Sprintf("%s:sql:%d", strings.TrimSuffix(filepath.Base(path), filepath.Ext(path)), idx),
			Program:      filepath.Base(path),
			SourcePath:   path,
			Chunk:        -30 - idx,
			Type:         "sql_statement",
			Content:      trimmed,
			Summary:      fmt.Sprintf("SQL statement: %s", headline),
			Technologies: []string{"SQL"},
		})
	}
	return docs, nil
}

func splitSQLStatements(data string) []string {
	parts := strings.Split(data, ";")
	var out []string
	for _, part := range parts {
		trimmed := strings.TrimSpace(part)
		if trimmed == "" {
			continue
		}
		out = append(out, trimmed+";")
	}
	return out
}
