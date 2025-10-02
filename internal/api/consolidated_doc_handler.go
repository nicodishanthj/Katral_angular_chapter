// File path: internal/api/consolidated_doc_handler.go
package api

import (
	"archive/zip"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strings"

	"github.com/nicodishanthj/Katral_phase1/internal/workflow"
)

func (s *Server) handleConsolidatedDoc(w http.ResponseWriter, r *http.Request) {
	projectID := strings.TrimSpace(r.URL.Query().Get("project_id"))
	if projectID == "" {
		writeError(w, http.StatusBadRequest, fmt.Errorf("project_id query parameter required"))
		return
	}
	artifactPath, err := s.workflow.ConsolidatedDocPath(projectID)
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

	if shouldDownloadConsolidatedDoc(r) {
		streamConsolidatedDoc(w, r, artifactPath)
		return
	}

	manifest, err := loadConsolidatedManifest(artifactPath)
	if err != nil {
		writeError(w, http.StatusInternalServerError, err)
		return
	}

	payload := map[string]interface{}{
		"artifact": map[string]string{
			"name": filepath.Base(artifactPath),
			"path": artifactPath,
		},
		"manifest": manifest,
	}
	writeJSON(w, http.StatusOK, payload)
}

func shouldDownloadConsolidatedDoc(r *http.Request) bool {
	downloadParam := strings.TrimSpace(r.URL.Query().Get("download"))
	if downloadParam != "" {
		switch strings.ToLower(downloadParam) {
		case "1", "true", "yes", "download", "zip":
			return true
		case "0", "false", "no":
			return false
		}
	}
	accept := strings.ToLower(r.Header.Get("Accept"))
	if accept != "" {
		if strings.Contains(accept, "application/zip") && !strings.Contains(accept, "application/json") {
			return true
		}
	}
	return false
}

func streamConsolidatedDoc(w http.ResponseWriter, r *http.Request, path string) {
	file, err := os.Open(path)
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
	name := filepath.Base(path)
	w.Header().Set("Content-Type", "application/zip")
	w.Header().Set("Content-Disposition", fmt.Sprintf("attachment; filename=\"%s\"", name))
	http.ServeContent(w, r, name, info.ModTime(), file)
}

func loadConsolidatedManifest(path string) (*workflow.ConsolidatedDoc, error) {
	archive, err := zip.OpenReader(path)
	if err != nil {
		return nil, fmt.Errorf("open consolidated artifact: %w", err)
	}
	defer archive.Close()
	for _, file := range archive.File {
		if file.FileInfo().IsDir() {
			continue
		}
		if !strings.EqualFold(filepath.Base(file.Name), "manifest.json") {
			continue
		}
		reader, err := file.Open()
		if err != nil {
			return nil, fmt.Errorf("open consolidated manifest: %w", err)
		}
		data, err := io.ReadAll(reader)
		reader.Close()
		if err != nil {
			return nil, fmt.Errorf("read consolidated manifest: %w", err)
		}
		var manifest workflow.ConsolidatedDoc
		if err := json.Unmarshal(data, &manifest); err != nil {
			return nil, fmt.Errorf("decode consolidated manifest: %w", err)
		}
		return &manifest, nil
	}
	return nil, fmt.Errorf("consolidated manifest not found in artifact")
}
