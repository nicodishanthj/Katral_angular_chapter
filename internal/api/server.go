// File path: internal/api/server.go
package api

import (
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"time"

	chi "github.com/go-chi/chi/v5"

	"github.com/nicodishanthj/Katral_phase1/internal/agent"
	"github.com/nicodishanthj/Katral_phase1/internal/common"
	ctxbuilder "github.com/nicodishanthj/Katral_phase1/internal/context"
	"github.com/nicodishanthj/Katral_phase1/internal/data/orchestrator"
	"github.com/nicodishanthj/Katral_phase1/internal/graph"
	graphmem "github.com/nicodishanthj/Katral_phase1/internal/graph/memory"
	"github.com/nicodishanthj/Katral_phase1/internal/llm"
	"github.com/nicodishanthj/Katral_phase1/internal/memory"
	"github.com/nicodishanthj/Katral_phase1/internal/metadata"
	"github.com/nicodishanthj/Katral_phase1/internal/retriever"
	"github.com/nicodishanthj/Katral_phase1/internal/vector"
	"github.com/nicodishanthj/Katral_phase1/internal/workflow"
)

type Server struct {
	router     chi.Router
	store      *memory.Store
	metadata   metadata.Store
	retriever  *retriever.Retriever
	provider   llm.Provider
	agent      *agent.Runner
	vector     vector.Store
	graph      graph.Client
	workflow   *workflow.Manager
	uploadRoot string
	ctxBuilder ctxbuilder.Builder

	orchestrator *orchestrator.Orchestrator
}

// Config controls how the API server processes documents during ingestion.
type Config struct {
	RewriteDocTypes []string
	EmbedDocTypes   []string
	UploadRoot      string
}

// DefaultConfig returns the standard configuration used when no overrides are
// provided.
func DefaultConfig() Config {
	return Config{
		RewriteDocTypes: []string{"metadata", "flow", "working_notes", "business_rules", "modernization"},
		EmbedDocTypes:   nil,
		UploadRoot:      filepath.Join(os.TempDir(), "katral_uploads"),
	}
}

// Merge overlays non-empty configuration slices from the override onto the
// base configuration.
func (c Config) Merge(override Config) Config {
	result := c
	if len(override.RewriteDocTypes) > 0 {
		result.RewriteDocTypes = append([]string(nil), override.RewriteDocTypes...)
	}
	if override.EmbedDocTypes != nil {
		result.EmbedDocTypes = append([]string(nil), override.EmbedDocTypes...)
	}
	if strings.TrimSpace(override.UploadRoot) != "" {
		result.UploadRoot = strings.TrimSpace(override.UploadRoot)
	}
	return result
}

func NewServer(ctx context.Context, orch *orchestrator.Orchestrator, provider llm.Provider, cfg *Config) (*Server, error) {
	logger := common.Logger()
	if orch == nil {
		return nil, fmt.Errorf("orchestrator required")
	}
	store := orch.Memory()
	if store == nil {
		return nil, fmt.Errorf("memory store unavailable")
	}
	catalog := orch.Catalog()
	if catalog == nil {
		return nil, fmt.Errorf("catalog store unavailable")
	}
	vectorClient := orch.Vector()
	graphClient := orch.Graph()
	docs, err := store.AllDocs(ctx, "")
	if err != nil {
		logger.Error("api: failed to load docs from store", "error", err)
		return nil, err
	}
	configuration := DefaultConfig()
	if cfg != nil {
		configuration = configuration.Merge(*cfg)
	}
	providerName := "unknown"
	if provider != nil {
		providerName = provider.Name()
	}
	logger.Info(
		"api: building server",
		"docs", len(docs),
		"provider", providerName,
		"vector_available", vectorClient != nil && vectorClient.Available(),
		"graph_available", graphClient != nil && graphClient.Available(),
	)
	depService := graphmem.NewService()
	depService.Refresh(docs)
	retr := retriever.New(
		docs,
		graphClient,
		retriever.WithDependencyService(depService),
		retriever.WithCacheSize(256),
	)
	ctxCfg := ctxbuilder.DefaultConfig()
	ctxBuilder, err := ctxbuilder.NewBuilder(ctxCfg, vectorClient, orch.Metadata(), retr, depService)
	if err != nil {
		return nil, fmt.Errorf("init context builder: %w", err)
	}
	rewriteDocTypes := buildDocTypeSet(configuration.RewriteDocTypes)
	embedDocTypes := buildDocTypeSet(configuration.EmbedDocTypes)
	manager := workflow.NewManager(store, provider, retr, ctxBuilder, vectorClient, graphClient, depService, catalog, rewriteDocTypes, embedDocTypes)

	uploadRoot := configuration.UploadRoot
	if strings.TrimSpace(uploadRoot) == "" {
		uploadRoot = filepath.Join(os.TempDir(), "katral_uploads")
	}
	if err := os.MkdirAll(uploadRoot, 0o755); err != nil {
		return nil, fmt.Errorf("create upload root: %w", err)
	}
	srv := &Server{
		router:       chi.NewRouter(),
		store:        store,
		metadata:     orch.Metadata(),
		retriever:    retr,
		provider:     provider,
		agent:        agent.NewRunner(provider, retr, ctxBuilder),
		vector:       vectorClient,
		graph:        graphClient,
		workflow:     manager,
		uploadRoot:   uploadRoot,
		ctxBuilder:   ctxBuilder,
		orchestrator: orch,
	}
	srv.routes()
	logger.Info("api: server ready", "routes", true)
	return srv, nil
}

func buildDocTypeSet(types []string) map[string]struct{} {
	if len(types) == 0 {
		return nil
	}
	out := make(map[string]struct{}, len(types))
	for _, t := range types {
		trimmed := strings.ToLower(strings.TrimSpace(t))
		if trimmed == "" {
			continue
		}
		out[trimmed] = struct{}{}
	}
	return out
}

func (s *Server) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	s.router.ServeHTTP(w, r)
}

// MetadataStore returns the backing metadata catalog interface.
func (s *Server) MetadataStore() metadata.Store {
	if s == nil {
		return nil
	}
	return s.metadata
}

func (s *Server) routes() {
	logger := common.Logger()
	logger.Info("api: configuring routes")
	s.router.Use(func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			start := time.Now()
			next.ServeHTTP(w, r)
			logger.Debug("request", "method", r.Method, "path", r.URL.Path, "dur", time.Since(start), "remote", r.RemoteAddr)
		})
	})

	s.router.Get("/healthz", func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		_, _ = w.Write([]byte("ok"))
	})

	uiPath := filepath.Join("web", "ui")
	if _, err := os.Stat(filepath.Join(uiPath, "index.html")); err != nil {
		logger.Warn("api: ui index missing", "path", filepath.Join(uiPath, "index.html"), "error", err)
	} else {
		logger.Info("api: ui assets located", "path", uiPath)
	}
	uiDir := http.Dir(uiPath)
	fileServer := http.FileServer(uiDir)
	s.router.Get("/ui", func(w http.ResponseWriter, r *http.Request) {
		http.Redirect(w, r, "/ui/", http.StatusMovedPermanently)
	})
	s.router.Get("/ui/*", func(w http.ResponseWriter, r *http.Request) {
		trimmed := strings.TrimPrefix(r.URL.Path, "/ui/")
		if trimmed == "" || trimmed == "/" {
			http.ServeFile(w, r, filepath.Join("web", "ui", "index.html"))
			return
		}
		http.StripPrefix("/ui/", fileServer).ServeHTTP(w, r)
	})

	s.router.Get("/", func(w http.ResponseWriter, r *http.Request) {
		http.Redirect(w, r, "/ui/", http.StatusFound)
	})

	s.router.Post("/v1/ingest", s.handleIngest)
	s.router.Post("/v1/ingest/upload", s.handleIngestUpload)
	s.router.Get("/v1/technologies", s.handleTechnologies)
	s.router.Get("/v1/search", s.handleSearch)
	s.router.Post("/v1/chat", s.handleChat)
	s.router.Post("/v1/agent/run", s.handleAgent)
	s.router.Post("/v1/workflow/start", s.handleWorkflowStart)
	s.router.Post("/v1/workflow/stop", s.handleWorkflowStop)
	s.router.Get("/v1/workflow/status", s.handleWorkflowStatus)
	s.router.Get("/v1/workflow/documentation", s.handleWorkflowDocumentation)
	s.router.Get("/v1/workflow/defaults", s.handleWorkflowDefaults)
	s.router.Get("/v1/workflow/download", s.handleWorkflowDownload)
	s.router.Get("/v1/logs", s.handleLogs)
	s.router.Get("/v1/projects", s.handleProjects)
	s.router.Get("/api/project-doc", s.handleProjectDoc)
	s.router.Get("/api/consolidated-doc", s.handleConsolidatedDoc)
	s.router.Get("/api/program-doc", s.handleProgramDoc)
}

func writeJSON(w http.ResponseWriter, status int, payload interface{}) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	_ = json.NewEncoder(w).Encode(payload)
}

func writeError(w http.ResponseWriter, status int, err error) {
	logger := common.Logger()
	if status >= http.StatusInternalServerError {
		logger.Error("request failed", "status", status, "error", err)
	} else {
		logger.Warn("request failed", "status", status, "error", err)
	}
	writeJSON(w, status, map[string]string{"error": err.Error()})
}
