// File path: cmd/cwa/services.go
package main

import (
	"context"
	"fmt"
	"log/slog"
	"net"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"github.com/nicodishanthj/Katral_phase1/internal/common/process"
)

func startIntegrationServices(ctx context.Context, logger *slog.Logger) ([]*process.ManagedService, error) {
	pythonBin, err := pythonBinary()
	if err != nil {
		return nil, err
	}
	workDir, err := os.Getwd()
	if err != nil {
		return nil, fmt.Errorf("resolve working directory: %w", err)
	}

	chromaDataDir := filepath.Join(workDir, "chroma_data")
	if err := os.MkdirAll(chromaDataDir, 0o755); err != nil {
		return nil, fmt.Errorf("prepare chroma data directory: %w", err)
	}
	kuzuDataDir := filepath.Join(workDir, "kuzu_data")
	if err := os.MkdirAll(kuzuDataDir, 0o755); err != nil {
		return nil, fmt.Errorf("prepare kuzu data directory: %w", err)
	}

	if err := ensureEnvDefault("CHROMADB_HOST", "127.0.0.1"); err != nil {
		return nil, err
	}
	if err := ensureEnvDefault("CHROMADB_PORT", "8000"); err != nil {
		return nil, err
	}
	if err := ensureEnvDefault("CHROMADB_SCHEME", "http"); err != nil {
		return nil, err
	}
	if err := ensureEnvDefault("CHROMADB_COLLECTION", "cwa_docs"); err != nil {
		return nil, err
	}
	if err := ensureEnvDefault("KUZU_ENDPOINT", "http://127.0.0.1:9000"); err != nil {
		return nil, err
	}
	if err := ensureEnvDefault("KUZU_DB_PATH", kuzuDataDir); err != nil {
		return nil, err
	}

	services := make([]*process.ManagedService, 0, 2)

	chromaHost := os.Getenv("CHROMADB_HOST")
	chromaPort := os.Getenv("CHROMADB_PORT")
	readyURL := fmt.Sprintf("%s://%s/api/v1/heartbeat", os.Getenv("CHROMADB_SCHEME"), net.JoinHostPort(chromaHost, chromaPort))
	chromaService, err := process.Start(ctx, process.ServiceConfig{
		Name:    "chromadb",
		Command: pythonBin,
		Args:    []string{filepath.Join("third_party", "python", "chromadb_server.py")},
		Env: []string{
			"PYTHONUNBUFFERED=1",
			fmt.Sprintf("CHROMADB_SERVER_HOST=%s", chromaHost),
			fmt.Sprintf("CHROMADB_SERVER_PORT=%s", chromaPort),
			fmt.Sprintf("CHROMADB_PERSIST_DIR=%s", chromaDataDir),
		},
		ReadyURL:     readyURL,
		ReadyTimeout: 2 * time.Minute,
		StopTimeout:  5 * time.Second,
		Logger:       logger.With("component", "launcher", "service", "chromadb"),
	})
	if err != nil {
		stopManagedServices(context.Background(), services, logger)
		return nil, err
	}
	services = append(services, chromaService)

	kuzuHost, kuzuPort := hostPortFromEndpoint(os.Getenv("KUZU_ENDPOINT"))
	kuzuReady := fmt.Sprintf("http://%s/health", net.JoinHostPort(kuzuHost, kuzuPort))
	kuzuService, err := process.Start(ctx, process.ServiceConfig{
		Name:    "kuzu",
		Command: pythonBin,
		Args:    []string{filepath.Join("third_party", "python", "kuzu_server.py")},
		Env: []string{
			"PYTHONUNBUFFERED=1",
			fmt.Sprintf("KUZU_SERVER_HOST=%s", kuzuHost),
			fmt.Sprintf("KUZU_SERVER_PORT=%s", kuzuPort),
			fmt.Sprintf("KUZU_DB_PATH=%s", kuzuDataDir),
		},
		ReadyURL:     kuzuReady,
		ReadyTimeout: 2 * time.Minute,
		StopTimeout:  5 * time.Second,
		Logger:       logger.With("component", "launcher", "service", "kuzu"),
	})
	if err != nil {
		stopManagedServices(context.Background(), services, logger)
		return nil, err
	}
	services = append(services, kuzuService)

	return services, nil
}

func stopManagedServices(ctx context.Context, services []*process.ManagedService, logger *slog.Logger) {
	for i := len(services) - 1; i >= 0; i-- {
		svc := services[i]
		if svc == nil {
			continue
		}
		if err := svc.Stop(ctx); err != nil && logger != nil {
			logger.Warn("launcher: service shutdown returned error", "error", err)
		}
	}
}

func pythonBinary() (string, error) {
	candidate := strings.TrimSpace(os.Getenv("PYTHON_BIN"))
	if candidate == "" {
		candidate = "python3"
	}
	path, err := process.BinaryPath(candidate)
	if err != nil {
		return "", fmt.Errorf("resolve python binary: %w", err)
	}
	return path, nil
}

func ensureEnvDefault(key, value string) error {
	if _, ok := os.LookupEnv(key); ok {
		return nil
	}
	if err := os.Setenv(key, value); err != nil {
		return fmt.Errorf("set %s: %w", key, err)
	}
	return nil
}

func hostPortFromEndpoint(endpoint string) (string, string) {
	trimmed := strings.TrimSpace(endpoint)
	if trimmed == "" {
		return "127.0.0.1", "9000"
	}
	if strings.HasPrefix(trimmed, "http://") || strings.HasPrefix(trimmed, "https://") {
		trimmed = strings.SplitN(trimmed, "://", 2)[1]
	}
	host, port, err := net.SplitHostPort(trimmed)
	if err != nil {
		if colon := strings.LastIndex(trimmed, ":"); colon >= 0 {
			host = trimmed[:colon]
			port = trimmed[colon+1:]
		} else {
			host = trimmed
			port = "9000"
		}
	}
	if strings.TrimSpace(host) == "" {
		host = "127.0.0.1"
	}
	if _, err := strconv.Atoi(port); err != nil {
		port = "9000"
	}
	return host, port
}
