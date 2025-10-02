// File path: internal/workflow/technology.go
package workflow

import (
	"context"
	"fmt"
	"os"
	"sort"
	"strings"

	"github.com/nicodishanthj/Katral_phase1/internal/kb"
)

// DetectTechnologies scans the provided repository and returns a sorted list of
// unique technologies discovered by the configured analyzers.
func (m *Manager) DetectTechnologies(ctx context.Context, repo string) ([]string, error) {
	repo = strings.TrimSpace(repo)
	if repo == "" {
		return nil, fmt.Errorf("repo path required")
	}
	info, err := os.Stat(repo)
	if err != nil {
		return nil, err
	}
	if !info.IsDir() {
		return nil, fmt.Errorf("repo must be a directory")
	}

	indexer := kb.NewIndexer()
	docs, err := indexer.IndexRepo(ctx, repo, nil)
	if err != nil {
		return nil, err
	}

	unique := make(map[string]string)
	for _, doc := range docs {
		for _, tech := range doc.Technologies {
			trimmed := strings.TrimSpace(tech)
			if trimmed == "" {
				continue
			}
			key := strings.ToLower(trimmed)
			if _, exists := unique[key]; !exists {
				unique[key] = trimmed
			}
		}
	}

	technologies := make([]string, 0, len(unique))
	for _, value := range unique {
		technologies = append(technologies, value)
	}
	sort.Slice(technologies, func(i, j int) bool {
		return strings.ToLower(technologies[i]) < strings.ToLower(technologies[j])
	})
	return technologies, nil
}
