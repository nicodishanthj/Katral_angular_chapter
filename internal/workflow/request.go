// File path: internal/workflow/request.go
package workflow

import (
	"fmt"
	"os"
	"strings"
)

func normalizeRequest(req Request) (Request, error) {
	req.ProjectID = strings.TrimSpace(req.ProjectID)
	if req.ProjectID == "" {
		return Request{}, fmt.Errorf("project id required")
	}

	if req.KnowledgeConfig != nil {
		req.KnowledgeConfig.Repo = strings.TrimSpace(req.KnowledgeConfig.Repo)
		stacks := make([]string, 0, len(req.KnowledgeConfig.Stacks))
		for _, stack := range req.KnowledgeConfig.Stacks {
			stack = strings.TrimSpace(stack)
			if stack != "" {
				stacks = append(stacks, stack)
			}
		}
		req.KnowledgeConfig.Stacks = stacks
		req.KnowledgeConfig.Collection = strings.TrimSpace(req.KnowledgeConfig.Collection)
	}

	if strings.TrimSpace(req.Mainframe) == "" && req.KnowledgeConfig != nil && req.KnowledgeConfig.Repo != "" {
		req.Mainframe = req.KnowledgeConfig.Repo
	}
	hasStacks := false
	for _, stack := range req.Stacks {
		if strings.TrimSpace(stack) != "" {
			hasStacks = true
			break
		}
	}
	if !hasStacks && req.KnowledgeConfig != nil && len(req.KnowledgeConfig.Stacks) > 0 {
		req.Stacks = append([]string(nil), req.KnowledgeConfig.Stacks...)
	}
	if strings.TrimSpace(req.Collection) == "" && req.KnowledgeConfig != nil && req.KnowledgeConfig.Collection != "" {
		req.Collection = req.KnowledgeConfig.Collection
	}

	req.Mainframe = strings.TrimSpace(req.Mainframe)
	if req.Mainframe == "" {
		return Request{}, fmt.Errorf("mainframe path required")
	}
	if _, err := os.Stat(req.Mainframe); err != nil {
		return Request{}, err
	}
	trimmedStacks := make([]string, 0, len(req.Stacks))
	for _, stack := range req.Stacks {
		stack = strings.TrimSpace(stack)
		if stack != "" {
			trimmedStacks = append(trimmedStacks, stack)
		}
	}
	if len(trimmedStacks) == 0 {
		trimmedStacks = []string{"cobol"}
	}
	req.Stacks = trimmedStacks
	req.Collection = strings.TrimSpace(req.Collection)
	req.GeneratorCommand = strings.TrimSpace(req.GeneratorCommand)
	req.GeneratorDir = strings.TrimSpace(req.GeneratorDir)
	req.SpringProject = strings.TrimSpace(req.SpringProject)
	req.ReviewPrompt = strings.TrimSpace(req.ReviewPrompt)
	flowValue := strings.TrimSpace(req.Flow)
	kind := resolveWorkflowKind(flowValue)
	req.Flow = string(kind)
	req.kind = kind
	if req.TargetConfig != nil {
		req.TargetConfig.Language = strings.TrimSpace(req.TargetConfig.Language)
		req.TargetConfig.Version = strings.TrimSpace(req.TargetConfig.Version)
		req.TargetConfig.Framework = strings.TrimSpace(req.TargetConfig.Framework)
		req.TargetConfig.Runtime = strings.TrimSpace(req.TargetConfig.Runtime)
		req.TargetConfig.Notes = strings.TrimSpace(req.TargetConfig.Notes)
	}
	if req.MigrationConfig != nil {
		req.MigrationConfig.AngularSourceRoot = strings.TrimSpace(req.MigrationConfig.AngularSourceRoot)
		req.MigrationConfig.AngularVersion = strings.TrimSpace(req.MigrationConfig.AngularVersion)
		req.MigrationConfig.ReactTargetRoot = strings.TrimSpace(req.MigrationConfig.ReactTargetRoot)
		req.MigrationConfig.ReactVersion = strings.TrimSpace(req.MigrationConfig.ReactVersion)

		if len(req.MigrationConfig.PatternMapping) > 0 {
			cleaned := make(map[string]string, len(req.MigrationConfig.PatternMapping))
			for pattern, mapping := range req.MigrationConfig.PatternMapping {
				key := strings.TrimSpace(pattern)
				value := strings.TrimSpace(mapping)
				if key != "" && value != "" {
					cleaned[key] = value
				}
			}
			if len(cleaned) == 0 {
				req.MigrationConfig.PatternMapping = nil
			} else {
				req.MigrationConfig.PatternMapping = cleaned
			}
		}

		if req.MigrationConfig.AngularSourceRoot == "" {
			return Request{}, fmt.Errorf("angular source root required for migration")
		}
		if req.MigrationConfig.AngularVersion == "" {
			return Request{}, fmt.Errorf("angular version required for migration")
		}
		if req.MigrationConfig.ReactTargetRoot == "" {
			return Request{}, fmt.Errorf("react target root required for migration")
		}
		if req.TargetConfig == nil || !strings.EqualFold(req.TargetConfig.Framework, "react") {
			return Request{}, fmt.Errorf("migration target framework must be react when migration_config is provided")
		}
		if req.MigrationConfig.ReactVersion != "" && req.TargetConfig.Version != "" && !strings.EqualFold(req.MigrationConfig.ReactVersion, req.TargetConfig.Version) {
			return Request{}, fmt.Errorf("react target version mismatch between migration and target configuration")
		}
	}
	return req, nil
}

func NormalizeRequest(req Request) (Request, error) {
	return normalizeRequest(req)
}

func resolveWorkflowKind(flow string) Kind {
	switch strings.ToLower(strings.TrimSpace(flow)) {
	case string(KindKnowledgeBase), "chat-with-code":
		return KindKnowledgeBase
	case string(KindDocumentGeneration), "document-generation":
		return KindDocumentGeneration
	case string(KindCodeConversion), "code_conversion", "codeconversion":
		return KindCodeConversion
	default:
		return KindCodeConversion
	}
}

func buildWorkflowSteps(kind Kind) []Step {
	switch kind {
	case KindKnowledgeBase:
		return []Step{
			{Name: "Index Knowledge Base", Status: StepPending},
			{Name: "Validate Knowledge Base", Status: StepPending},
		}
	case KindDocumentGeneration:
		return []Step{
			{Name: "Index Source Material", Status: StepPending},
			{Name: "Generate Documentation Package", Status: StepPending},
		}
	default:
		return []Step{
			{Name: "Ingestion", Status: StepPending},
			{Name: "Code Generation", Status: StepPending},
			{Name: "Package Spring Artifact", Status: StepPending},
			{Name: "Knowledge Review", Status: StepPending},
		}
	}
}

func formatTargetConfigDetails(cfg *TargetConfig) string {
	if cfg == nil {
		return ""
	}
	parts := make([]string, 0, 5)
	if cfg.Language != "" {
		parts = append(parts, fmt.Sprintf("language=%s", cfg.Language))
	}
	if cfg.Version != "" {
		parts = append(parts, fmt.Sprintf("version=%s", cfg.Version))
	}
	if cfg.Framework != "" {
		parts = append(parts, fmt.Sprintf("framework=%s", cfg.Framework))
	}
	if cfg.Runtime != "" {
		parts = append(parts, fmt.Sprintf("runtime=%s", cfg.Runtime))
	}
	if cfg.Notes != "" {
		parts = append(parts, fmt.Sprintf("notes=%s", cfg.Notes))
	}
	return strings.Join(parts, ", ")
}

func appendTargetDetails(message string, cfg *TargetConfig) string {
	details := formatTargetConfigDetails(cfg)
	if details == "" {
		return message
	}
	if message == "" {
		return fmt.Sprintf("Target: %s", details)
	}
	return fmt.Sprintf("%s (Target: %s)", message, details)
}
