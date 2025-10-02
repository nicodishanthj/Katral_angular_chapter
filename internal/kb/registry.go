// File path: internal/kb/registry.go
package kb

import (
	"context"

	"github.com/nicodishanthj/Katral_phase1/internal/kb/cobol"
)

type Analyzer interface {
	Name() string
	Match(path string, data []byte) bool
	Parse(ctx context.Context, path string, data []byte) ([]Doc, error)
}

func defaultAnalyzers() []Analyzer {
	return []Analyzer{
		&cobolAnalyzer{parser: cobol.NewParser()},
		&jclAnalyzer{},
		&imsAnalyzer{},
		&db2Analyzer{},
		&genericSQLAnalyzer{},
		&cicsAnalyzer{},
		&mqAnalyzer{},
		&asmAnalyzer{},
	}
}
