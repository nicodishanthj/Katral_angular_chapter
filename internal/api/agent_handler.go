// File path: internal/api/agent_handler.go
package api

import (
	"encoding/json"
	"fmt"
	"net/http"

	"github.com/nicodishanthj/Katral_phase1/internal/agent"
	"github.com/nicodishanthj/Katral_phase1/internal/common"
)

func (s *Server) handleAgent(w http.ResponseWriter, r *http.Request) {
	logger := common.Logger()
	var req agentRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		logger.Warn("api: agent decode failed", "error", err)
		writeError(w, http.StatusBadRequest, err)
		return
	}
	if req.Goal == "" {
		logger.Warn("api: agent goal missing")
		writeError(w, http.StatusBadRequest, fmt.Errorf("goal required"))
		return
	}
	logger.Info("api: agent run requested", "goal", req.Goal, "flow", req.Flow)
	var opts *agent.RunOptions
	if req.Flow != "" || req.KnowledgeConfig != nil || req.TargetConfig != nil {
		opts = &agent.RunOptions{
			Flow:      req.Flow,
			Knowledge: req.KnowledgeConfig,
			Target:    req.TargetConfig,
		}
	}
	result, err := s.agent.RunSmall(r.Context(), req.Goal, opts)
	if err != nil {
		logger.Error("api: agent run failed", "error", err)
		writeError(w, http.StatusInternalServerError, err)
		return
	}
	logger.Info("api: agent run completed")
	writeJSON(w, http.StatusOK, map[string]string{"result": result})
}
