// File path: internal/agent/graph.go
package agent

import (
	"context"
	"fmt"
	"strings"

	langchaingo "github.com/tmc/langchaingo"
	langgraphgo "github.com/tmc/langgraphgo"

	"github.com/nicodishanthj/Katral_phase1/internal/common"
	ctxbuilder "github.com/nicodishanthj/Katral_phase1/internal/context"
	"github.com/nicodishanthj/Katral_phase1/internal/kb"
	"github.com/nicodishanthj/Katral_phase1/internal/llm"
	"github.com/nicodishanthj/Katral_phase1/internal/retriever"
)

type Runner struct {
	provider  llm.Provider
	retriever *retriever.Retriever
	builder   ctxbuilder.Builder
}

func NewRunner(provider llm.Provider, retr *retriever.Retriever, ctxBuilder ctxbuilder.Builder) *Runner {
	return &Runner{provider: provider, retriever: retr, builder: ctxBuilder}
}

type KnowledgeConfig struct {
	Repo       string   `json:"repo,omitempty"`
	Stacks     []string `json:"stacks,omitempty"`
	Collection string   `json:"collection,omitempty"`
}

type TargetConfig struct {
	Language  string `json:"language,omitempty"`
	Version   string `json:"version,omitempty"`
	Framework string `json:"framework,omitempty"`
	Runtime   string `json:"runtime,omitempty"`
	Notes     string `json:"notes,omitempty"`
}

type RunOptions struct {
	Flow      string           `json:"flow,omitempty"`
	Knowledge *KnowledgeConfig `json:"knowledge_config,omitempty"`
	Target    *TargetConfig    `json:"target_config,omitempty"`
}

func (r *Runner) RunSmall(ctx context.Context, goal string, opts *RunOptions) (string, error) {
	graph := langgraphgo.NewGraph(func(ctx context.Context, goal string) (string, error) {
		if r.provider == nil {
			return fmt.Sprintf("no agent provider available: %s", goal), nil
		}
		systemPrompt := buildSystemPrompt(opts)
		messages := []llm.Message{{Role: "system", Content: systemPrompt}}
		if ctxMsgs := r.collectKnowledgeMessages(ctx, goal, opts); len(ctxMsgs) > 0 {
			messages = append(messages, ctxMsgs...)
		}
		messages = append(messages, llm.Message{Role: "user", Content: goal})
		// Record for debugging using langchaingo.Message slice.
		_ = []langchaingo.Message{{Role: "system", Content: "agent"}}
		return r.provider.Chat(ctx, messages)
	})
	return graph.Run(ctx, goal)
}

func buildSystemPrompt(opts *RunOptions) string {
	systemPrompt := "You are a concise automation agent."
	if opts == nil {
		return systemPrompt
	}
	var contextParts []string
	if opts.Flow != "" {
		contextParts = append(contextParts, fmt.Sprintf("Active flow: %s.", opts.Flow))
		if strings.EqualFold(strings.TrimSpace(opts.Flow), "angular-react-migration") {
			contextParts = append(contextParts,
				"Focus areas: deliver migration complexity assessments, recommend React patterns to replace Angular constructs, review migrated component code for parity, and compare Angular versus React performance considerations.")
		}
	}
	if kc := opts.Knowledge; kc != nil {
		var segments []string
		if kc.Repo != "" {
			segments = append(segments, fmt.Sprintf("repository=%s", kc.Repo))
		}
		if len(kc.Stacks) > 0 {
			segments = append(segments, fmt.Sprintf("stacks=%s", strings.Join(kc.Stacks, ", ")))
		}
		if kc.Collection != "" {
			segments = append(segments, fmt.Sprintf("collection=%s", kc.Collection))
		}
		if len(segments) > 0 {
			contextParts = append(contextParts, fmt.Sprintf("Knowledge configuration: %s.", strings.Join(segments, ", ")))
		}
	}
	if tc := opts.Target; tc != nil {
		var segments []string
		if tc.Language != "" {
			segments = append(segments, fmt.Sprintf("language=%s", tc.Language))
		}
		if tc.Version != "" {
			segments = append(segments, fmt.Sprintf("version=%s", tc.Version))
		}
		if tc.Framework != "" {
			segments = append(segments, fmt.Sprintf("framework=%s", tc.Framework))
		}
		if tc.Runtime != "" {
			segments = append(segments, fmt.Sprintf("runtime=%s", tc.Runtime))
		}
		if tc.Notes != "" {
			segments = append(segments, fmt.Sprintf("notes=%s", tc.Notes))
		}
		if len(segments) > 0 {
			contextParts = append(contextParts, fmt.Sprintf("Target modernization: %s.", strings.Join(segments, ", ")))
		}
	}
	if len(contextParts) > 0 {
		systemPrompt = fmt.Sprintf("%s %s", systemPrompt, strings.Join(contextParts, " "))
	}
	return systemPrompt
}

func (r *Runner) collectKnowledgeMessages(ctx context.Context, goal string, opts *RunOptions) []llm.Message {
	snippets := r.collectKnowledgeSnippets(ctx, goal, opts)
	if len(snippets) == 0 {
		return nil
	}
	messages := make([]llm.Message, 0, len(snippets))
	for _, snippet := range snippets {
		messages = append(messages, llm.Message{Role: "system", Content: snippet})
	}
	return messages
}

func (r *Runner) collectKnowledgeSnippets(ctx context.Context, goal string, opts *RunOptions) []string {
	if goal == "" {
		return nil
	}
	const limit = 5
	query := goal
	if opts != nil {
		if extra := describeKnowledgeQuery(opts); extra != "" {
			query = fmt.Sprintf("%s\n%s", goal, extra)
		}
	}
	var snippets []knowledgeSnippet
	seen := make(map[string]struct{})
	logger := common.Logger()
	if r.builder != nil {
		req := ctxbuilder.GoalRequest{Query: query, Limit: limit, Embedder: r.provider}
		result, err := r.builder.BuildGoalContext(ctx, req)
		if err != nil {
			logger.Warn("agent: context builder failed", "error", err)
		} else {
			for _, snip := range result.Snippets {
				text := strings.TrimSpace(snip.Content)
				if text == "" {
					text = strings.TrimSpace(snip.Summary)
				}
				if text == "" {
					continue
				}
				snippet := knowledgeSnippet{
					ID:      snip.ID,
					Program: snip.Program,
					Source:  snip.Source,
					Summary: snip.Summary,
					Content: text,
				}
				if graphSummary := summarizeGraphContext(snip.Graph); graphSummary != "" {
					snippet.Content = fmt.Sprintf("%s\n\nGraph context:\n%s", snippet.Content, graphSummary)
				}
				key := snippet.dedupeKey()
				if _, exists := seen[key]; exists {
					continue
				}
				seen[key] = struct{}{}
				snippets = append(snippets, snippet)
				if len(snippets) >= limit {
					break
				}
			}
		}
	}
	if len(snippets) < limit && r.retriever != nil {
		fallback := r.retriever.Search(query, limit)
		for _, res := range fallback {
			doc := res.Doc
			text := doc.Content
			if strings.TrimSpace(text) == "" {
				text = doc.Summary
			}
			if strings.TrimSpace(text) == "" {
				continue
			}
			snippet := knowledgeSnippet{
				ID:      doc.ID,
				Program: doc.Program,
				Source:  doc.SourcePath,
				Summary: doc.Summary,
				Content: text,
			}
			if graphSummary := summarizeGraphContext(res.Graph); graphSummary != "" {
				snippet.Content = fmt.Sprintf("%s\n\nGraph context:\n%s", snippet.Content, graphSummary)
			}
			key := snippet.dedupeKey()
			if _, exists := seen[key]; exists {
				continue
			}
			seen[key] = struct{}{}
			snippets = append(snippets, snippet)
			if len(snippets) >= limit {
				break
			}
		}
	}
	if len(snippets) == 0 {
		return nil
	}
	if len(snippets) > limit {
		snippets = snippets[:limit]
	}
	return formatKnowledgeSnippets(snippets)
}

func describeKnowledgeQuery(opts *RunOptions) string {
	if opts == nil {
		return ""
	}
	var parts []string
	if opts.Flow != "" {
		parts = append(parts, fmt.Sprintf("Flow:%s", opts.Flow))
		if strings.EqualFold(strings.TrimSpace(opts.Flow), "angular-react-migration") {
			parts = append(parts, "Focus:MigrationComplexity;PatternRecommendations;MigratedComponentCodeReview;PerformanceComparison")
		}
	}
	if kc := opts.Knowledge; kc != nil {
		if kc.Repo != "" {
			parts = append(parts, fmt.Sprintf("Repo:%s", kc.Repo))
		}
		if len(kc.Stacks) > 0 {
			parts = append(parts, fmt.Sprintf("Stacks:%s", strings.Join(kc.Stacks, ", ")))
		}
		if kc.Collection != "" {
			parts = append(parts, fmt.Sprintf("Collection:%s", kc.Collection))
		}
	}
	if tc := opts.Target; tc != nil {
		if tc.Language != "" {
			parts = append(parts, fmt.Sprintf("TargetLanguage:%s", tc.Language))
		}
		if tc.Framework != "" {
			parts = append(parts, fmt.Sprintf("TargetFramework:%s", tc.Framework))
		}
	}
	if len(parts) == 0 {
		return ""
	}
	return "Context:" + strings.Join(parts, "; ")
}

type knowledgeSnippet struct {
	ID      string
	Program string
	Source  string
	Summary string
	Content string
}

func summarizeGraphContext(ctx ctxbuilder.GraphContext) string {
	if len(ctx.Dependencies) == 0 && len(ctx.Impacts) == 0 && len(ctx.Related) == 0 {
		return ""
	}
	sections := make([]string, 0, 4)
	if deps := summarizeNeighborGroup(ctx.Dependencies, 3); deps != "" {
		sections = append(sections, "Dependencies: "+deps)
	}
	if impacts := summarizeNeighborGroup(ctx.Impacts, 3); impacts != "" {
		sections = append(sections, "Impacts: "+impacts)
	}
	if related := summarizeNeighborGroup(ctx.Related, 3); related != "" {
		sections = append(sections, "Related: "+related)
	}
	if ctx.Signal != 0 {
		sections = append(sections, fmt.Sprintf("Graph signal: %.2f", ctx.Signal))
	}
	return strings.Join(sections, "\n")
}

func summarizeNeighborGroup(neighbors []kb.GraphNeighbor, limit int) string {
	if len(neighbors) == 0 {
		return ""
	}
	max := limit
	if max <= 0 || max > len(neighbors) {
		max = len(neighbors)
	}
	names := make([]string, 0, max+1)
	for i := 0; i < max; i++ {
		neighbor := neighbors[i]
		label := strings.TrimSpace(neighbor.Program)
		displayName := strings.TrimSpace(neighbor.Name)
		if displayName != "" && !strings.EqualFold(displayName, label) {
			label = fmt.Sprintf("%s (%s)", label, displayName)
		}
		if len(neighbor.Chain) > 1 {
			label = fmt.Sprintf("%s via %s", label, strings.Join(neighbor.Chain[1:], " -> "))
		}
		names = append(names, label)
	}
	if extra := len(neighbors) - max; extra > 0 {
		names = append(names, fmt.Sprintf("+%d more", extra))
	}
	return strings.Join(names, "; ")
}

func (k knowledgeSnippet) dedupeKey() string {
	fields := []string{
		strings.ToLower(strings.TrimSpace(k.ID)),
		strings.ToLower(strings.TrimSpace(k.Program)),
		strings.ToLower(strings.TrimSpace(k.Source)),
		strings.ToLower(strings.TrimSpace(k.Content)),
	}
	return strings.Join(fields, "|")
}

func formatKnowledgeSnippets(snippets []knowledgeSnippet) []string {
	formatted := make([]string, 0, len(snippets))
	for idx, snippet := range snippets {
		text := strings.TrimSpace(snippet.Content)
		if text == "" {
			continue
		}
		var builder strings.Builder
		builder.WriteString(fmt.Sprintf("[Snippet %d]", idx+1))
		var meta []string
		if snippet.Program != "" {
			meta = append(meta, "Program: "+snippet.Program)
		}
		if snippet.Source != "" {
			meta = append(meta, "Source: "+snippet.Source)
		}
		if len(meta) > 0 {
			builder.WriteString(" ")
			builder.WriteString(strings.Join(meta, " | "))
		}
		builder.WriteString("\n")
		if snippet.Summary != "" && snippet.Summary != text {
			builder.WriteString("Summary: ")
			builder.WriteString(snippet.Summary)
			builder.WriteString("\n")
		}
		builder.WriteString(trimSnippet(text, 900))
		formatted = append(formatted, builder.String())
	}
	return formatted
}

func trimSnippet(text string, limit int) string {
	cleaned := strings.TrimSpace(text)
	if limit <= 0 {
		return cleaned
	}
	runes := []rune(cleaned)
	if len(runes) <= limit {
		return cleaned
	}
	trimmed := strings.TrimSpace(string(runes[:limit]))
	if trimmed == "" {
		return cleaned
	}
	return trimmed + "…"
}
