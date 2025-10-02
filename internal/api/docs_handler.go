// File path: internal/api/docs_handler.go
package api

import (
	"fmt"
	"net/http"
	"strings"

	"github.com/nicodishanthj/Katral_phase1/internal/common"
	ctxbuilder "github.com/nicodishanthj/Katral_phase1/internal/context"
	"github.com/nicodishanthj/Katral_phase1/internal/llm"
	"github.com/nicodishanthj/Katral_phase1/internal/vector"
)

func (s *Server) handleProjectDoc(w http.ResponseWriter, r *http.Request) {
	common.Logger().Debug("api: project doc requested")
	projectID := strings.TrimSpace(r.URL.Query().Get("project_id"))
	if projectID == "" {
		writeError(w, http.StatusBadRequest, fmt.Errorf("project_id query parameter required"))
		return
	}
	var embedder llm.Provider
	var store vector.Store
	if s.vector != nil {
		store = s.vector
		embedder = s.provider
		if embedder == nil {
			embedder = llm.NewProvider()
		}
	}
	doc := s.retriever.ProjectDoc(r.Context(), projectID, embedder, store)
	writeJSON(w, http.StatusOK, doc)
}

func (s *Server) handleProgramDoc(w http.ResponseWriter, r *http.Request) {
	logger := common.Logger()
	symbol := r.URL.Query().Get("symbol")
	if symbol == "" {
		logger.Warn("api: program doc missing symbol")
		writeError(w, http.StatusBadRequest, fmt.Errorf("symbol required"))
		return
	}
	projectID := strings.TrimSpace(r.URL.Query().Get("project_id"))
	if projectID == "" {
		logger.Warn("api: program doc missing project")
		writeError(w, http.StatusBadRequest, fmt.Errorf("project_id query parameter required"))
		return
	}
	var embedder llm.Provider
	var store vector.Store
	if s.vector != nil {
		store = s.vector
		embedder = s.provider
		if embedder == nil {
			embedder = llm.NewProvider()
		}
	}
	if s.ctxBuilder != nil {
		req := ctxbuilder.ProgramRequest{Program: symbol, ProjectID: projectID, Embedder: embedder}
		result, err := s.ctxBuilder.BuildProgramContext(r.Context(), req)
		if err == nil && result.Document != nil {
			logger.Info("api: program doc returned via builder", "symbol", symbol)
			writeJSON(w, http.StatusOK, result.Document)
			return
		} else if err != nil {
			logger.Warn("api: program doc builder failed", "symbol", symbol, "error", err)
		}
	}
	doc, ok := s.retriever.ProgramDoc(r.Context(), projectID, symbol, embedder, store)
	if !ok {
		logger.Warn("api: program doc not found", "symbol", symbol)
		writeError(w, http.StatusNotFound, fmt.Errorf("program not found"))
		return
	}
	logger.Info("api: program doc returned", "symbol", symbol, "project", projectID, "vector", store != nil && embedder != nil)
	writeJSON(w, http.StatusOK, doc)
}
