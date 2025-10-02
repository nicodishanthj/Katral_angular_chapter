// File path: internal/api/ingest_handler.go
package api

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strings"

	"github.com/nicodishanthj/Katral_phase1/internal/common"
)

func (s *Server) handleIngest(w http.ResponseWriter, r *http.Request) {
	logger := common.Logger()
	ctx := r.Context()
	var req ingestRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		logger.Warn("api: ingest decode failed", "error", err)
		writeError(w, http.StatusBadRequest, err)
		return
	}
	req.ProjectID = strings.TrimSpace(req.ProjectID)
	if req.ProjectID == "" {
		writeError(w, http.StatusBadRequest, fmt.Errorf("project_id is required"))
		return
	}
	logger.Info("api: ingest requested", "repo", req.Repo, "stacks", len(req.Stacks), "collection", req.Collection, "project", req.ProjectID)
	resp, err := s.ingestRepo(ctx, req, false)
	if err != nil {
		status := http.StatusInternalServerError
		if os.IsNotExist(err) || os.IsPermission(err) {
			status = http.StatusBadRequest
		}
		logger.Error("api: ingest failed", "status", status, "error", err)
		writeError(w, status, err)
		return
	}
	logger.Info("api: ingest succeeded", "count", resp.Count, "collection", resp.Collection)
	writeJSON(w, http.StatusOK, resp)
}

func (s *Server) handleIngestUpload(w http.ResponseWriter, r *http.Request) {
	logger := common.Logger()
	ctx := r.Context()
	const maxMemory = 64 << 20 // 64 MiB of in-memory file parts
	if err := r.ParseMultipartForm(maxMemory); err != nil {
		logger.Warn("api: ingest upload form parse failed", "error", err)
		writeError(w, http.StatusBadRequest, fmt.Errorf("failed to parse upload form: %w", err))
		return
	}
	if r.MultipartForm != nil {
		defer r.MultipartForm.RemoveAll()
	}

	repoInput := strings.TrimSpace(r.FormValue("repo"))
	repo := repoInput
	tempRepo := false
	if repo == "" {
		workspace, err := os.MkdirTemp(s.uploadRoot, "workspace-")
		if err != nil {
			writeError(w, http.StatusInternalServerError, fmt.Errorf("create workspace: %w", err))
			return
		}
		repo = workspace
		tempRepo = true
		defer func() {
			if tempRepo {
				if err := os.RemoveAll(repo); err != nil {
					logger.Warn("api: cleanup workspace failed", "workspace", repo, "error", err)
				}
			}
		}()
	} else {
		info, err := os.Stat(repo)
		if err != nil {
			status := http.StatusBadRequest
			if !os.IsNotExist(err) {
				status = http.StatusInternalServerError
			}
			writeError(w, status, fmt.Errorf("stat repo: %w", err))
			return
		}
		if !info.IsDir() {
			writeError(w, http.StatusBadRequest, fmt.Errorf("repo must be a directory"))
			return
		}
	}

	files := r.MultipartForm.File["files"]
	if len(files) == 0 {
		writeError(w, http.StatusBadRequest, fmt.Errorf("no files provided"))
		return
	}

	var saved int
	for _, fileHeader := range files {
		name := strings.TrimSpace(fileHeader.Filename)
		if name == "" {
			writeError(w, http.StatusBadRequest, fmt.Errorf("file name required"))
			return
		}
		cleaned := filepath.Clean(name)
		if cleaned == "." || cleaned == "" || filepath.IsAbs(cleaned) {
			writeError(w, http.StatusBadRequest, fmt.Errorf("invalid file path: %s", name))
			return
		}
		destPath := filepath.Join(repo, cleaned)
		rel, relErr := filepath.Rel(repo, destPath)
		if relErr != nil || strings.HasPrefix(rel, "..") {
			writeError(w, http.StatusBadRequest, fmt.Errorf("invalid file path: %s", name))
			return
		}
		if err := os.MkdirAll(filepath.Dir(destPath), 0o755); err != nil {
			writeError(w, http.StatusInternalServerError, fmt.Errorf("create directories: %w", err))
			return
		}
		src, err := fileHeader.Open()
		if err != nil {
			writeError(w, http.StatusInternalServerError, fmt.Errorf("open uploaded file: %w", err))
			return
		}
		dst, err := os.OpenFile(destPath, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0o644)
		if err != nil {
			_ = src.Close()
			writeError(w, http.StatusInternalServerError, fmt.Errorf("create destination file: %w", err))
			return
		}
		if _, err := io.Copy(dst, src); err != nil {
			_ = dst.Close()
			_ = src.Close()
			writeError(w, http.StatusInternalServerError, fmt.Errorf("write destination file: %w", err))
			return
		}
		if err := dst.Close(); err != nil {
			_ = src.Close()
			writeError(w, http.StatusInternalServerError, fmt.Errorf("close destination file: %w", err))
			return
		}
		if err := src.Close(); err != nil {
			writeError(w, http.StatusInternalServerError, fmt.Errorf("close uploaded file: %w", err))
			return
		}
		saved++
	}

	projectID := strings.TrimSpace(r.FormValue("project_id"))
	if projectID == "" {
		writeError(w, http.StatusBadRequest, fmt.Errorf("project_id is required"))
		return
	}
	ingestReq := ingestRequest{
		Repo:       repo,
		Collection: strings.TrimSpace(r.FormValue("collection")),
		ProjectID:  projectID,
	}
	ingestResp, err := s.ingestRepo(ctx, ingestReq, false)
	if err != nil {
		status := http.StatusInternalServerError
		if os.IsNotExist(err) || os.IsPermission(err) {
			status = http.StatusBadRequest
		}
		writeError(w, status, err)
		return
	}

	resp := ingestUploadResponse{
		Uploaded:  saved,
		Documents: ingestResp.Count,
	}
	if !tempRepo {
		resp.Repo = repo
	} else {
		resp.Workspace = filepath.Base(repo)
	}
	if ingestResp.Collection != "" {
		resp.Collection = ingestResp.Collection
	} else if ingestReq.Collection != "" {
		resp.Collection = ingestReq.Collection
	}
	if ingestResp.Warning != "" {
		resp.Warning = ingestResp.Warning
	}

	logFields := []interface{}{"files", saved, "documents", ingestResp.Count}
	if tempRepo {
		logFields = append(logFields, "workspace", repo)
	} else {
		logFields = append(logFields, "repo", repo)
	}
	logger.Info("api: ingest upload succeeded", logFields...)
	writeJSON(w, http.StatusOK, resp)
}

func (s *Server) ingestRepo(ctx context.Context, req ingestRequest, replace bool) (ingestResponse, error) {
	return s.workflow.Ingest(ctx, req, replace)
}
