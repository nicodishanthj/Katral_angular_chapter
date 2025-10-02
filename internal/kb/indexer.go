// File path: internal/kb/indexer.go
package kb

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
)

type Indexer struct {
	analyzers []Analyzer
}

func NewIndexer() *Indexer {
	return &Indexer{analyzers: defaultAnalyzers()}
}

func (i *Indexer) IndexRepo(ctx context.Context, repoPath string, stacks []string) ([]Doc, error) {
	if len(i.analyzers) == 0 {
		return nil, errors.New("no analyzers configured")
	}
	var collected []Doc
	insights := newModernizationInsights()
	err := filepath.WalkDir(repoPath, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			return nil
		}
		data, readErr := os.ReadFile(path)
		if readErr != nil {
			return fmt.Errorf("read file %s: %w", path, readErr)
		}
		matched := false
		for _, analyzer := range i.analyzers {
			if !analyzer.Match(path, data) {
				continue
			}
			docs, parseErr := analyzer.Parse(ctx, path, data)
			if parseErr != nil {
				return fmt.Errorf("parse file %s with %s: %w", path, analyzer.Name(), parseErr)
			}
			if len(docs) == 0 {
				continue
			}
			matched = true
			for _, doc := range docs {
				collected = append(collected, doc)
				insights.Observe(doc)
			}
		}
		if !matched {
			// treat plain text as documentation if explicitly requested via stacks
			if hasStack(stacks, "plaintext") {
				collected = append(collected, Doc{
					ID:         fmt.Sprintf("%s:raw", strings.TrimSuffix(path, filepath.Ext(path))),
					Program:    filepath.Base(path),
					SourcePath: path,
					Chunk:      0,
					Type:       "raw",
					Content:    string(data),
					Summary:    fmt.Sprintf("Raw content from %s", path),
				})
			}
		}
		return nil
	})
	if err != nil {
		return nil, err
	}
	modernizationDocs := buildModernizationDocs(stacks, insights)
	collected = append(collected, modernizationDocs...)
	return collected, nil
}
