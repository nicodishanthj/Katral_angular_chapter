// File path: internal/api/chat_handler.go
package api

import (
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"strings"

	"github.com/nicodishanthj/Katral_phase1/internal/common"
	ctxbuilder "github.com/nicodishanthj/Katral_phase1/internal/context"
	"github.com/nicodishanthj/Katral_phase1/internal/kb"
	"github.com/nicodishanthj/Katral_phase1/internal/llm"
)

func (s *Server) handleChat(w http.ResponseWriter, r *http.Request) {
	logger := common.Logger()
	ctx := r.Context()
	var req chatRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		logger.Warn("api: chat decode failed", "error", err)
		writeError(w, http.StatusBadRequest, err)
		return
	}
	if req.Prompt == "" {
		logger.Warn("api: chat prompt missing")
		writeError(w, http.StatusBadRequest, fmt.Errorf("prompt required"))
		return
	}
	logger.Info("api: chat request received", "prompt_length", len(req.Prompt), "use_rag", req.UseRAG)
	provider := s.provider
	if provider == nil {
		provider = llm.NewProvider()
		logger.Debug("api: chat created new provider instance", "provider", provider.Name())
	}
	baseSystemPrompt := "You are Katral, an expert mainframe modernization assistant. " +
		"Respond with clear, actionable guidance tailored to COBOL and legacy workloads. " +
		"Use Markdown with short sections and bullet lists when helpful. " +
		"Ground every answer in the provided context snippets when they exist, referencing them by their [Snippet #] labels. " +
		"If the context is insufficient, state the limitation before hypothesizing."
	messages := []llm.Message{{Role: "system", Content: baseSystemPrompt}}
	var contextSnippets []string
	if req.UseRAG {
		contextSnippets = s.collectContext(ctx, provider, req.Prompt, 5)
		if len(contextSnippets) > 0 {
			contextBlock := "Context snippets:\n" + strings.Join(contextSnippets, "\n\n")
			messages = append(messages, llm.Message{Role: "system", Content: contextBlock})
		}
	}
	messages = append(messages, llm.Message{Role: "user", Content: req.Prompt})
	answer, err := provider.Chat(ctx, messages)
	if err != nil {
		logger.Error("api: chat completion failed", "error", err)
		writeError(w, http.StatusInternalServerError, err)
		return
	}
	logger.Info("api: chat completion succeeded", "provider", provider.Name())
	writeJSON(w, http.StatusOK, map[string]interface{}{
		"answer":   answer,
		"context":  contextSnippets,
		"provider": provider.Name(),
	})
}

func (s *Server) collectContext(ctx context.Context, provider llm.Provider, query string, limit int) []string {
	logger := common.Logger()
	logger.Debug("api: collecting context", "query", query, "limit", limit)
	if limit <= 0 {
		limit = 5
	}
	var snippets []contextSnippet
	seen := make(map[string]struct{})
	if s.ctxBuilder != nil && provider != nil {
		req := ctxbuilder.GoalRequest{Query: query, Limit: limit, Embedder: provider}
		result, err := s.ctxBuilder.BuildGoalContext(ctx, req)
		if err != nil {
			logger.Error("api: builder context failed", "error", err)
		} else {
			for _, snip := range result.Snippets {
				text := strings.TrimSpace(snip.Content)
				if text == "" {
					text = strings.TrimSpace(snip.Summary)
				}
				if text == "" {
					continue
				}
				snippet := contextSnippet{
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
	if len(snippets) < limit && s.retriever != nil {
		fallback := s.retriever.Search(query, limit)
		logger.Debug("api: context using fallback retriever", "fallback_results", len(fallback))
		for _, res := range fallback {
			doc := res.Doc
			text := doc.Content
			if text == "" {
				text = doc.Summary
			}
			if text == "" {
				continue
			}
			snippet := contextSnippet{
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
	if len(snippets) > limit {
		snippets = snippets[:limit]
	}
	return formatContextSnippets(snippets)
}

type contextSnippet struct {
	ID      string
	Program string
	Source  string
	Summary string
	Content string
}

func (c contextSnippet) dedupeKey() string {
	keyParts := []string{strings.ToLower(strings.TrimSpace(c.ID)), strings.ToLower(strings.TrimSpace(c.Program)), strings.ToLower(strings.TrimSpace(c.Source)), strings.ToLower(strings.TrimSpace(c.Content))}
	return strings.Join(keyParts, "|")
}

func formatContextSnippets(snippets []contextSnippet) []string {
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
		builder.WriteString(trimText(text, 900))
		formatted = append(formatted, builder.String())
	}
	return formatted
}

func trimText(text string, limit int) string {
	cleaned := strings.TrimSpace(text)
	runes := []rune(cleaned)
	if limit <= 0 || len(runes) <= limit {
		return cleaned
	}
	trimmed := strings.TrimSpace(string(runes[:limit]))
	if trimmed == "" {
		return cleaned
	}
	return trimmed + "…"
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
