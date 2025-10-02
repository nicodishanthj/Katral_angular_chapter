// File path: internal/api/technology_handler.go
package api

import (
	"fmt"
	"net/http"
	"os"
	"strings"

	"github.com/nicodishanthj/Katral_phase1/internal/common"
)

type technologyResponse struct {
	Technologies []string `json:"technologies"`
}

func (s *Server) handleTechnologies(w http.ResponseWriter, r *http.Request) {
	logger := common.Logger()
	repo := strings.TrimSpace(r.URL.Query().Get("repo"))
	if repo == "" {
		writeError(w, http.StatusBadRequest, fmt.Errorf("repo path required"))
		return
	}
	techs, err := s.workflow.DetectTechnologies(r.Context(), repo)
	if err != nil {
		status := http.StatusInternalServerError
		if os.IsNotExist(err) || os.IsPermission(err) {
			status = http.StatusBadRequest
		}
		logger.Warn("api: technology detection failed", "repo", repo, "error", err, "status", status)
		writeError(w, status, err)
		return
	}
	logger.Info("api: technologies detected", "repo", repo, "count", len(techs))
	writeJSON(w, http.StatusOK, technologyResponse{Technologies: techs})
}
