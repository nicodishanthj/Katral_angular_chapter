// File path: internal/api/search_handler.go
package api

import (
	"fmt"
	"net/http"
	"strconv"

	"github.com/nicodishanthj/Katral_phase1/internal/common"
	ctxbuilder "github.com/nicodishanthj/Katral_phase1/internal/context"
	"github.com/nicodishanthj/Katral_phase1/internal/llm"
	"github.com/nicodishanthj/Katral_phase1/internal/vector"
	"github.com/nicodishanthj/Katral_phase1/internal/workflow"
)

func (s *Server) handleSearch(w http.ResponseWriter, r *http.Request) {
	logger := common.Logger()
	query := r.URL.Query().Get("q")
	if query == "" {
		logger.Warn("api: search missing query parameter")
		writeError(w, http.StatusBadRequest, fmt.Errorf("missing q parameter"))
		return
	}
	limit := 5
	if v := r.URL.Query().Get("limit"); v != "" {
		if parsed, err := strconv.Atoi(v); err == nil {
			limit = parsed
		}
	}
	projectID := r.URL.Query().Get("project_id")
	requestedCollection := r.URL.Query().Get("collection")
	collectionName := workflow.ProjectCollectionName(projectID, requestedCollection)
	logger.Info("api: search request", "query", query, "limit", limit, "project", projectID)
	provider := s.provider
	if provider == nil {
		provider = llm.NewProvider()
	}
	if s.ctxBuilder != nil {
		req := ctxbuilder.GoalRequest{Query: query, Limit: limit, Embedder: provider, ProjectID: projectID, Collection: collectionName}
		result, err := s.ctxBuilder.BuildGoalContext(r.Context(), req)
		if err != nil {
			logger.Error("api: context builder search failed", "error", err)
		} else if len(result.Snippets) > 0 {
			logger.Debug("api: search served via context builder", "results", len(result.Snippets))
			writeJSON(w, http.StatusOK, map[string]interface{}{
				"results":    convertSnippets(result.Snippets),
				"collection": resolveCollection(result.Collection, s.vector, collectionName),
			})
			return
		}
	}
	fallback := s.fallbackSearch(query, limit)
	logger.Debug("api: search served via fallback", "results", len(fallback))
	writeJSON(w, http.StatusOK, map[string]interface{}{
		"results":    fallback,
		"collection": "memory",
	})
}

type searchPoint struct {
	ID      string                 `json:"id"`
	Score   float32                `json:"score"`
	Payload map[string]interface{} `json:"payload"`
}

func convertPoints(points []vector.SearchResult) []searchPoint {
	out := make([]searchPoint, 0, len(points))
	for _, pt := range points {
		out = append(out, searchPoint{
			ID:      pt.ID,
			Score:   pt.Score,
			Payload: pt.Payload,
		})
	}
	return out
}

func convertSnippets(snippets []ctxbuilder.Snippet) []searchPoint {
	out := make([]searchPoint, 0, len(snippets))
	for _, snip := range snippets {
		payload := map[string]interface{}{
			"program": snip.Program,
			"source":  snip.Source,
			"summary": snip.Summary,
			"content": snip.Content,
		}
		if snip.Metadata != nil {
			payload["metadata"] = snip.Metadata
		}
		if snip.Graph.Program != "" {
			payload["graph"] = snip.Graph
		}
		out = append(out, searchPoint{
			ID:      snip.ID,
			Score:   float32(snip.Score),
			Payload: payload,
		})
	}
	return out
}

func resolveCollection(builderCollection string, store vector.Store, requested string) string {
	if builderCollection != "" {
		return builderCollection
	}
	if store != nil {
		return store.Collection()
	}
	if requested != "" {
		return requested
	}
	return "memory"
}

func (s *Server) fallbackSearch(query string, limit int) []searchPoint {
	common.Logger().Debug("api: executing fallback search", "query", query, "limit", limit)
	results := s.retriever.Search(query, limit)
	out := make([]searchPoint, 0, len(results))
	for _, res := range results {
		doc := res.Doc
		payload := map[string]interface{}{
			"program":      doc.Program,
			"source":       doc.SourcePath,
			"content":      doc.Content,
			"summary":      doc.Summary,
			"technologies": doc.Technologies,
		}
		if res.Graph.Program != "" {
			payload["graph"] = res.Graph
		}
		out = append(out, searchPoint{
			ID:      doc.ID,
			Score:   float32(res.Score),
			Payload: payload,
		})
	}
	return out
}
