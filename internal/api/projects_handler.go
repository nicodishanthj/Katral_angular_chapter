// File path: internal/api/projects_handler.go
package api

import (
	"fmt"
	"net/http"
	"sort"
	"strings"

	"github.com/nicodishanthj/Katral_phase1/internal/workflow"
)

type projectSummary struct {
	ID        string          `json:"project_id"`
	Documents int             `json:"documents"`
	State     *workflow.State `json:"state,omitempty"`
}

func (s *Server) handleProjects(w http.ResponseWriter, r *http.Request) {
	ctx := r.Context()
	infos, err := s.store.Projects(ctx)
	if err != nil {
		writeError(w, http.StatusInternalServerError, fmt.Errorf("list projects: %w", err))
		return
	}
	counts := make(map[string]int, len(infos))
	ids := make(map[string]struct{}, len(infos))
	for _, info := range infos {
		trimmed := strings.TrimSpace(info.ID)
		if trimmed == "" {
			continue
		}
		counts[trimmed] = info.Documents
		ids[trimmed] = struct{}{}
	}
	states := s.workflow.ProjectStates()
	for id := range states {
		ids[id] = struct{}{}
	}
	ordered := make([]string, 0, len(ids))
	for id := range ids {
		ordered = append(ordered, id)
	}
	sort.Strings(ordered)
	summaries := make([]projectSummary, 0, len(ordered))
	for _, id := range ordered {
		summary := projectSummary{ID: id, Documents: counts[id]}
		if state, ok := states[id]; ok {
			stateCopy := state
			summary.State = &stateCopy
		}
		summaries = append(summaries, summary)
	}
	writeJSON(w, http.StatusOK, map[string]interface{}{"projects": summaries})
}
