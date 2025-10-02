// File path: internal/workflow/generator_integration.go
package workflow

import (
	"context"
	"path/filepath"
	"strings"

	"github.com/nicodishanthj/Katral_phase1/internal/workflow/generator"
)

func (m *Manager) generateSpringProject(ctx context.Context, projectID string, req Request) (string, error) {
	if m.metadata == nil {
		m.AppendLog("warn", "Spring generator unavailable: metadata store not configured")
		return "", nil
	}
	options := generator.Options{
		ProjectID: projectID,
	}
	if trimmed := strings.TrimSpace(req.SpringProject); trimmed != "" {
		options.TargetDir = trimmed
	} else if trimmed := strings.TrimSpace(req.GeneratorDir); trimmed != "" {
		options.TargetDir = trimmed
	}
	gen := generator.NewSpringGenerator(m.metadata, m.ctx, m.AppendLog)
	path, err := gen.Generate(ctx, options)
	if err != nil {
		return "", err
	}
	absPath, err := filepath.Abs(path)
	if err != nil {
		return "", err
	}
	m.AppendLog("info", "%s located at %s", appendTargetDetails("Generated Spring Boot project", req.TargetConfig), absPath)
	return absPath, nil
}
