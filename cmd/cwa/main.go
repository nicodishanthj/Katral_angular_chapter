// File path: cmd/cwa/main.go
package main

import (
	"context"
	"flag"
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"github.com/joho/godotenv"
	"github.com/nicodishanthj/Katral_phase1/internal/api"
	"github.com/nicodishanthj/Katral_phase1/internal/common"
	"github.com/nicodishanthj/Katral_phase1/internal/data/orchestrator"
	"github.com/nicodishanthj/Katral_phase1/internal/llm"
)

func main() {
	logger := common.Logger()

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	if err := godotenv.Load(); err != nil {
		logger.Warn("katral: .env file not loaded", "error", err)
	} else {
		logger.Info("katral: environment loaded from .env")
	}

	addr := flag.String("addr", ":8081", "listen address")
	storePath := flag.String("store", defaultStorePath(), "path to the knowledge base store")
	rewriteDefault := strings.Join(api.DefaultConfig().RewriteDocTypes, ",")
	if env := strings.TrimSpace(os.Getenv("KATRAL_REWRITE_TYPES")); env != "" {
		rewriteDefault = env
	}
	embedDefault := strings.TrimSpace(os.Getenv("KATRAL_EMBED_TYPES"))
	rewriteTypes := flag.String("rewrite-types", rewriteDefault, "comma-separated doc types eligible for summary rewriting")
	embedTypes := flag.String("embed-types", embedDefault, "comma-separated doc types eligible for embeddings (empty for all)")
	catalogPath := flag.String("catalog", defaultCatalogPath(), "path to the SQLite catalog database")
	syncInterval := flag.String("sync-interval", "", "interval between background reconciliation cycles (e.g. 30s, 2m)")
	syncTimeout := flag.String("sync-timeout", "", "timeout for a single reconciliation cycle")
	syncRetries := flag.Int("sync-retries", -1, "maximum retry attempts for a failed reconciliation cycle (-1 uses defaults)")
	syncBackoff := flag.String("sync-backoff", "", "base backoff duration between reconciliation retries")

	autoStartDefault := true
	if env := strings.TrimSpace(os.Getenv("KATRAL_AUTOSTART")); env != "" {
		if parsed, err := strconv.ParseBool(env); err == nil {
			autoStartDefault = parsed
		}
	}
	autoStartIntegrations := flag.Bool("auto-start-integrations", autoStartDefault, "automatically launch bundled ChromaDB and Kuzu helpers")

	flag.Parse()

	logger.Info("katral: startup initiated", "addr", *addr, "store", *storePath)

	if *autoStartIntegrations {
		services, serviceErr := startIntegrationServices(ctx, logger)
		if serviceErr != nil {
			logger.Error("katral: failed to launch integrations", "error", serviceErr)
			fmt.Println("integration startup error:", serviceErr)
			os.Exit(1)
		}
		defer stopManagedServices(context.Background(), services, logger)
	}

	orchCfg, err := orchestrator.LoadConfig()
	if err != nil {
		logger.Error("katral: orchestrator config load failed", "error", err)
		fmt.Println("orchestrator config error:", err)
		os.Exit(1)
	}
	if trimmed := strings.TrimSpace(*storePath); trimmed != "" {
		orchCfg.MemoryPath = trimmed
	}
	if trimmed := strings.TrimSpace(*catalogPath); trimmed != "" {
		orchCfg.SQLitePath = trimmed
	}
	if trimmed := strings.TrimSpace(*syncInterval); trimmed != "" {
		dur, err := time.ParseDuration(trimmed)
		if err != nil {
			logger.Error("katral: invalid sync interval", "value", trimmed, "error", err)
			fmt.Println("sync interval error:", err)
			os.Exit(1)
		}
		orchCfg.SyncInterval = dur
	}
	if trimmed := strings.TrimSpace(*syncTimeout); trimmed != "" {
		dur, err := time.ParseDuration(trimmed)
		if err != nil {
			logger.Error("katral: invalid sync timeout", "value", trimmed, "error", err)
			fmt.Println("sync timeout error:", err)
			os.Exit(1)
		}
		orchCfg.SyncTimeout = dur
	}
	if *syncRetries >= 0 {
		orchCfg.MaxSyncRetries = *syncRetries
	}
	if trimmed := strings.TrimSpace(*syncBackoff); trimmed != "" {
		dur, err := time.ParseDuration(trimmed)
		if err != nil {
			logger.Error("katral: invalid sync backoff", "value", trimmed, "error", err)
			fmt.Println("sync backoff error:", err)
			os.Exit(1)
		}
		orchCfg.RetryBackoff = dur
	}

	orch, err := orchestrator.New(ctx, orchCfg)
	if err != nil {
		logger.Error("katral: orchestrator initialization failed", "error", err)
		fmt.Println("orchestrator error:", err)
		os.Exit(1)
	}
	defer orch.Close()

	provider := llm.NewProvider()
	logger.Info("katral: llm provider ready", "provider", provider.Name())

	vectorClient := orch.Vector()
	if vectorClient != nil {
		if vectorClient.Available() {
			logger.Info("katral: chromadb available", "collection", vectorClient.Collection())
		} else {
			logger.Warn("katral: chromadb unreachable", "collection", vectorClient.Collection())
		}
	} else {
		logger.Info("katral: chromadb not configured")
	}

	graphClient := orch.Graph()
	if graphClient != nil {
		if graphClient.Available() {
			logger.Info("katral: kuzu available")
		} else {
			logger.Warn("katral: kuzu unreachable")
		}
	} else {
		logger.Info("katral: kuzu graph not configured")
	}

	cfg := api.DefaultConfig()
	if trimmed := strings.TrimSpace(*rewriteTypes); trimmed != "" {
		cfg.RewriteDocTypes = parseDocTypes(trimmed)
	}
	if trimmed := strings.TrimSpace(*embedTypes); trimmed != "" {
		cfg.EmbedDocTypes = parseDocTypes(trimmed)
	}

	server, err := api.NewServer(ctx, orch, provider, &cfg)
	if err != nil {
		logger.Error("katral: server construction failed", "error", err)
		fmt.Println("server error:", err)
		os.Exit(1)
	}

	logger.Info("katral: server listening", "addr", *addr, "ui", "/ui/", "health", "/healthz")
	fmt.Printf("Serving on %s\n", *addr)
	reachable := *addr
	if strings.HasPrefix(reachable, ":") {
		reachable = "localhost" + reachable
	}
	logger.Info("katral: verify reachability", "suggestion", fmt.Sprintf("curl http://%s/healthz", reachable))
	if err := http.ListenAndServe(*addr, server); err != nil {
		logger.Error("katral: server stopped", "error", err)
		fmt.Println("server stopped:", err)
	}
}

func defaultStorePath() string {
	return filepath.Join("data", "docs.jsonl")
}

func defaultCatalogPath() string {
	return filepath.Join("data", "catalog.db")
}

func parseDocTypes(value string) []string {
	if value == "" {
		return nil
	}
	parts := strings.Split(value, ",")
	out := make([]string, 0, len(parts))
	for _, part := range parts {
		trimmed := strings.TrimSpace(part)
		if trimmed == "" {
			continue
		}
		out = append(out, trimmed)
	}
	return out
}
