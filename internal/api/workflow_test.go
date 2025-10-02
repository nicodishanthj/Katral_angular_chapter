// File path: internal/api/workflow_test.go
package api

import (
	"reflect"
	"testing"

	"github.com/nicodishanthj/Katral_phase1/internal/workflow"
)

func TestNormalizeWorkflowRequestPopulatesFromKnowledgeConfig(t *testing.T) {
	tempDir := t.TempDir()

	req := workflow.Request{
		ProjectID: " project ",
		KnowledgeConfig: &workflow.KnowledgeConfig{
			Repo:       "  " + tempDir + "  ",
			Stacks:     []string{" cobol ", ""},
			Collection: "  collection  ",
		},
	}

	normalized, err := workflow.NormalizeRequest(req)
	if err != nil {
		t.Fatalf("normalizeWorkflowRequest returned error: %v", err)
	}

	if normalized.ProjectID != "project" {
		t.Fatalf("expected project id to be trimmed, got %q", normalized.ProjectID)
	}

	if normalized.Mainframe != tempDir {
		t.Fatalf("expected mainframe to default from knowledge config repo %q, got %q", tempDir, normalized.Mainframe)
	}

	expectedStacks := []string{"cobol"}
	if !reflect.DeepEqual(normalized.Stacks, expectedStacks) {
		t.Fatalf("expected stacks %v, got %v", expectedStacks, normalized.Stacks)
	}

	if normalized.Collection != "collection" {
		t.Fatalf("expected collection to default from knowledge config, got %q", normalized.Collection)
	}

	if normalized.KnowledgeConfig == nil {
		t.Fatalf("expected knowledge config to remain populated")
	}

	if normalized.KnowledgeConfig.Repo != tempDir {
		t.Fatalf("expected knowledge config repo to be trimmed, got %q", normalized.KnowledgeConfig.Repo)
	}
	if !reflect.DeepEqual(normalized.KnowledgeConfig.Stacks, expectedStacks) {
		t.Fatalf("expected knowledge config stacks %v, got %v", expectedStacks, normalized.KnowledgeConfig.Stacks)
	}
	if normalized.KnowledgeConfig.Collection != "collection" {
		t.Fatalf("expected knowledge config collection to be trimmed, got %q", normalized.KnowledgeConfig.Collection)
	}
}
