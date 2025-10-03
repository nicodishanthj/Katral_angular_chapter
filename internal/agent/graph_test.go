package agent

import (
	"strings"
	"testing"
)

func TestBuildSystemPromptAngularMigration(t *testing.T) {
	prompt := buildSystemPrompt(&RunOptions{Flow: "angular-react-migration"})
	if !strings.Contains(prompt, "You are a concise automation agent.") {
		t.Fatalf("expected base system prompt, got %q", prompt)
	}
	expected := "Focus areas: deliver migration complexity assessments"
	if !strings.Contains(prompt, expected) {
		t.Fatalf("expected migration focus in prompt: %q", prompt)
	}
}

func TestDescribeKnowledgeQueryAngularMigration(t *testing.T) {
	query := describeKnowledgeQuery(&RunOptions{Flow: "angular-react-migration"})
	if !strings.Contains(query, "Flow:angular-react-migration") {
		t.Fatalf("expected flow marker in query: %q", query)
	}
	for _, token := range []string{
		"MigrationComplexity",
		"PatternRecommendations",
		"MigratedComponentCodeReview",
		"PerformanceComparison",
	} {
		if !strings.Contains(query, token) {
			t.Fatalf("expected %q in query: %q", token, query)
		}
	}
}
