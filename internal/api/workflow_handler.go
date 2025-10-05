// File path: internal/api/workflow_handler.go
package api

import (
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"github.com/nicodishanthj/Katral_phase1/internal/common"
	"github.com/nicodishanthj/Katral_phase1/internal/workflow"
)

func (s *Server) handleWorkflowStart(w http.ResponseWriter, r *http.Request) {
	var req workflow.Request
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		writeError(w, http.StatusBadRequest, err)
		return
	}
	if err := s.workflow.Start(req); err != nil {
		status := http.StatusBadRequest
		switch {
		case errors.Is(err, workflow.ErrWorkflowRunning):
			status = http.StatusConflict
		case errors.Is(err, os.ErrNotExist), errors.Is(err, os.ErrPermission):
			status = http.StatusBadRequest
		case errors.Is(err, workflow.ErrWorkflowNotFound):
			status = http.StatusNotFound
		default:
			if strings.Contains(err.Error(), "required") {
				status = http.StatusBadRequest
			} else {
				status = http.StatusInternalServerError
			}
		}
		writeError(w, status, err)
		return
	}
	writeJSON(w, http.StatusAccepted, map[string]string{"status": "started"})
}

func (s *Server) handleWorkflowStop(w http.ResponseWriter, r *http.Request) {
	var req struct {
		ProjectID string `json:"project_id"`
	}
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		writeError(w, http.StatusBadRequest, err)
		return
	}
	projectID := strings.TrimSpace(req.ProjectID)
	if projectID == "" {
		writeError(w, http.StatusBadRequest, fmt.Errorf("project_id is required"))
		return
	}
	if err := s.workflow.Stop(projectID); err != nil {
		status := http.StatusInternalServerError
		switch {
		case errors.Is(err, workflow.ErrWorkflowNotFound):
			status = http.StatusNotFound
		case errors.Is(err, workflow.ErrWorkflowNotRunning):
			status = http.StatusConflict
		default:
			if strings.Contains(err.Error(), "required") {
				status = http.StatusBadRequest
			}
		}
		writeError(w, status, err)
		return
	}
	writeJSON(w, http.StatusAccepted, map[string]string{"status": "stopping"})
}

func (s *Server) handleWorkflowStatus(w http.ResponseWriter, r *http.Request) {
	projectID := strings.TrimSpace(r.URL.Query().Get("project_id"))
	if projectID == "" {
		writeError(w, http.StatusBadRequest, fmt.Errorf("project_id query parameter required"))
		return
	}
	state := s.workflow.Status(projectID)
	writeJSON(w, http.StatusOK, state)
}

func (s *Server) handleWorkflowDocumentation(w http.ResponseWriter, r *http.Request) {
	projectID := strings.TrimSpace(r.URL.Query().Get("project_id"))
	if projectID == "" {
		writeError(w, http.StatusBadRequest, fmt.Errorf("project_id query parameter required"))
		return
	}
	artifacts, err := s.workflow.LoadDocumentationArtifacts(projectID)
	if err != nil {
		status := http.StatusInternalServerError
		switch {
		case errors.Is(err, os.ErrNotExist), errors.Is(err, workflow.ErrArtifactNotFound):
			status = http.StatusNotFound
		default:
			if strings.Contains(err.Error(), "project id required") {
				status = http.StatusBadRequest
			}
		}
		writeError(w, status, err)
		return
	}
	writeJSON(w, http.StatusOK, map[string]interface{}{"artifacts": artifacts})
}

func (s *Server) handleWorkflowDefaults(w http.ResponseWriter, r *http.Request) {
	logger := common.Logger()
	cwd, err := os.Getwd()
	if err != nil {
		logger.Warn("api: resolve working directory", "error", err)
		cwd = ""
	}

	type candidateSet map[string]time.Time

	addCandidate := func(set candidateSet, value string, ts time.Time) {
		if set == nil {
			return
		}
		trimmed := strings.TrimSpace(value)
		if trimmed == "" {
			return
		}
		if existing, ok := set[trimmed]; !ok || ts.After(existing) {
			set[trimmed] = ts
		}
	}

	sortCandidates := func(set candidateSet) []string {
		if len(set) == 0 {
			return nil
		}
		type entry struct {
			value string
			ts    time.Time
		}
		entries := make([]entry, 0, len(set))
		for value, ts := range set {
			entries = append(entries, entry{value: value, ts: ts})
		}
		sort.SliceStable(entries, func(i, j int) bool {
			if entries[i].ts.Equal(entries[j].ts) {
				return entries[i].value < entries[j].value
			}
			return entries[i].ts.After(entries[j].ts)
		})
		ordered := make([]string, 0, len(entries))
		for _, entry := range entries {
			ordered = append(ordered, entry.value)
		}
		return ordered
	}

	projectCandidates := make(candidateSet)
	mainframeCandidates := make(candidateSet)
	collectionCandidates := make(candidateSet)
	generatorCandidates := make(candidateSet)
	generatorDirCandidates := make(candidateSet)
	springCandidates := make(candidateSet)
	reviewCandidates := make(candidateSet)
	flowCandidates := make(candidateSet)

	now := time.Now()
	if cwd != "" {
		addCandidate(mainframeCandidates, cwd, now.Add(-2*time.Millisecond))
		sampleDirs := []string{
			filepath.Join(cwd, "examples", "mainframe"),
			filepath.Join(cwd, "examples", "cobol"),
		}
		for idx, dir := range sampleDirs {
			info, statErr := os.Stat(dir)
			if statErr != nil || !info.IsDir() {
				continue
			}
			addCandidate(mainframeCandidates, dir, now.Add(time.Duration(idx+1)*time.Millisecond))
		}
	}

	states := make(map[string]workflow.State)
	if s != nil && s.workflow != nil {
		states = s.workflow.ProjectStates()
	}

	for id, state := range states {
		ts := time.Time{}
		switch {
		case state.Running:
			ts = now
		case state.CompletedAt != nil && !state.CompletedAt.IsZero():
			ts = state.CompletedAt.UTC()
		case state.StartedAt != nil && !state.StartedAt.IsZero():
			ts = state.StartedAt.UTC()
		}

		addCandidate(projectCandidates, id, ts)
		req := state.Request
		addCandidate(projectCandidates, req.ProjectID, ts)
		addCandidate(mainframeCandidates, req.Mainframe, ts)
		addCandidate(collectionCandidates, req.Collection, ts)
		addCandidate(generatorCandidates, req.GeneratorCommand, ts)
		addCandidate(generatorDirCandidates, req.GeneratorDir, ts)
		addCandidate(springCandidates, req.SpringProject, ts)
		addCandidate(reviewCandidates, req.ReviewPrompt, ts)
		addCandidate(flowCandidates, req.Flow, ts)

		if req.KnowledgeConfig != nil {
			addCandidate(mainframeCandidates, req.KnowledgeConfig.Repo, ts)
			addCandidate(collectionCandidates, req.KnowledgeConfig.Collection, ts)
		}
	}

	response := map[string]interface{}{
		"working_directory":  cwd,
		"project_ids":        sortCandidates(projectCandidates),
		"mainframe_roots":    sortCandidates(mainframeCandidates),
		"collections":        sortCandidates(collectionCandidates),
		"generator_commands": sortCandidates(generatorCandidates),
		"generator_dirs":     sortCandidates(generatorDirCandidates),
		"spring_projects":    sortCandidates(springCandidates),
		"review_prompts":     sortCandidates(reviewCandidates),
		"flows":              sortCandidates(flowCandidates),
	}

	writeJSON(w, http.StatusOK, response)
}

func (s *Server) handleWorkflowDownload(w http.ResponseWriter, r *http.Request) {
	projectID := strings.TrimSpace(r.URL.Query().Get("project_id"))
	if projectID == "" {
		writeError(w, http.StatusBadRequest, fmt.Errorf("project_id query parameter required"))
		return
	}
	artifactKind := strings.TrimSpace(r.URL.Query().Get("artifact"))
	if artifactKind == "" {
		artifactKind = "spring"
	}
	var artifactPath string
	var err error
	switch {
	case strings.EqualFold(artifactKind, "spring"):
		artifactPath, err = s.workflow.SpringArtifactPath(projectID)
	default:
		artifactPath, err = s.workflow.DocumentationArtifactPath(projectID, artifactKind)
		if errors.Is(err, workflow.ErrArtifactNotFound) {
			artifactPath, err = s.workflow.ConversionArtifactPath(projectID, artifactKind)
		}
	}
	if err != nil {
		status := http.StatusInternalServerError
		switch {
		case errors.Is(err, workflow.ErrArtifactNotFound):
			status = http.StatusNotFound
		case errors.Is(err, workflow.ErrArtifactInvalid):
			status = http.StatusForbidden
		default:
			if errors.Is(err, os.ErrNotExist) {
				status = http.StatusNotFound
			}
		}
		writeError(w, status, err)
		return
	}
	file, err := os.Open(artifactPath)
	if err != nil {
		status := http.StatusInternalServerError
		if errors.Is(err, os.ErrNotExist) {
			status = http.StatusNotFound
		}
		writeError(w, status, err)
		return
	}
	defer file.Close()
	info, err := file.Stat()
	if err != nil {
		writeError(w, http.StatusInternalServerError, err)
		return
	}
	name := filepath.Base(artifactPath)
	w.Header().Set("Content-Type", detectContentType(name))
	w.Header().Set("Content-Disposition", fmt.Sprintf("attachment; filename=\"%s\"", name))
	http.ServeContent(w, r, name, info.ModTime(), file)
}

func detectContentType(name string) string {
	switch strings.ToLower(filepath.Ext(strings.TrimSpace(name))) {
	case ".md":
		return "text/markdown"
	case ".html", ".htm":
		return "text/html"
	case ".json":
		return "application/json"
	case ".txt":
		return "text/plain"
	default:
		return "application/octet-stream"
	}
}

func (s *Server) handleLogs(w http.ResponseWriter, r *http.Request) {
	combined := append([]common.LogEntry(nil), common.LogEntries()...)
	existing := make(map[string]struct{}, len(combined))
	for _, entry := range combined {
		existing[logEntryKey(entry.Time, entry.Level, entry.Message, entry.Component)] = struct{}{}
	}

	workflowEntries := s.workflow.Logs()
	for _, entry := range workflowEntries {
		converted := common.LogEntry{
			Time:      entry.Time,
			Level:     strings.ToLower(entry.Level),
			Message:   entry.Message,
			Component: "workflow",
		}
		key := logEntryKey(converted.Time, converted.Level, converted.Message, converted.Component)
		if _, ok := existing[key]; ok {
			continue
		}
		combined = append(combined, converted)
		existing[key] = struct{}{}
	}

	sort.SliceStable(combined, func(i, j int) bool {
		if combined[i].Time.Equal(combined[j].Time) {
			if combined[i].Component == combined[j].Component {
				if combined[i].Level == combined[j].Level {
					return combined[i].Message < combined[j].Message
				}
				return combined[i].Level < combined[j].Level
			}
			return combined[i].Component < combined[j].Component
		}
		return combined[i].Time.Before(combined[j].Time)
	})

	writeJSON(w, http.StatusOK, map[string]interface{}{"entries": combined})
}

func logEntryKey(ts time.Time, level, message, component string) string {
	stamp := ts.UTC().Format(time.RFC3339Nano)
	return strings.Join([]string{stamp, strings.ToLower(strings.TrimSpace(level)), strings.TrimSpace(component), message}, "|")
}
