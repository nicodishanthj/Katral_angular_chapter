// File path: internal/data/orchestrator/config.go
package orchestrator

import (
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"
)

// Config controls the construction of the orchestrator and its background
// synchronisation loop.
type Config struct {
	MemoryPath     string
	SQLitePath     string
	SyncInterval   time.Duration
	SyncTimeout    time.Duration
	MaxSyncRetries int
	RetryBackoff   time.Duration
}

// DefaultConfig returns the baseline configuration used when no overrides are
// supplied.
func DefaultConfig() Config {
	return Config{
		MemoryPath:     filepath.Join("data", "docs.jsonl"),
		SQLitePath:     filepath.Join("data", "catalog.db"),
		SyncInterval:   time.Minute,
		SyncTimeout:    30 * time.Second,
		MaxSyncRetries: 3,
		RetryBackoff:   5 * time.Second,
	}
}

// LoadConfig builds a Config from defaults and environment variables.
func LoadConfig() (Config, error) {
	cfg := DefaultConfig()
	if value := strings.TrimSpace(os.Getenv("KATRAL_MEMORY_PATH")); value != "" {
		cfg.MemoryPath = value
	}
	if value := strings.TrimSpace(os.Getenv("KATRAL_CATALOG_PATH")); value != "" {
		cfg.SQLitePath = value
	}
	if value := strings.TrimSpace(os.Getenv("KATRAL_SYNC_INTERVAL")); value != "" {
		dur, err := time.ParseDuration(value)
		if err != nil {
			return Config{}, fmt.Errorf("parse KATRAL_SYNC_INTERVAL: %w", err)
		}
		cfg.SyncInterval = dur
	}
	if value := strings.TrimSpace(os.Getenv("KATRAL_SYNC_TIMEOUT")); value != "" {
		dur, err := time.ParseDuration(value)
		if err != nil {
			return Config{}, fmt.Errorf("parse KATRAL_SYNC_TIMEOUT: %w", err)
		}
		cfg.SyncTimeout = dur
	}
	if value := strings.TrimSpace(os.Getenv("KATRAL_SYNC_RETRIES")); value != "" {
		retries, err := strconv.Atoi(value)
		if err != nil {
			return Config{}, fmt.Errorf("parse KATRAL_SYNC_RETRIES: %w", err)
		}
		if retries < 0 {
			retries = 0
		}
		cfg.MaxSyncRetries = retries
	}
	if value := strings.TrimSpace(os.Getenv("KATRAL_SYNC_BACKOFF")); value != "" {
		dur, err := time.ParseDuration(value)
		if err != nil {
			return Config{}, fmt.Errorf("parse KATRAL_SYNC_BACKOFF: %w", err)
		}
		cfg.RetryBackoff = dur
	}
	return applyDefaults(cfg), nil
}

func applyDefaults(cfg Config) Config {
	defaults := DefaultConfig()
	if strings.TrimSpace(cfg.MemoryPath) == "" {
		cfg.MemoryPath = defaults.MemoryPath
	}
	if strings.TrimSpace(cfg.SQLitePath) == "" {
		cfg.SQLitePath = defaults.SQLitePath
	}
	if cfg.SyncInterval <= 0 {
		cfg.SyncInterval = defaults.SyncInterval
	}
	if cfg.SyncTimeout <= 0 {
		cfg.SyncTimeout = defaults.SyncTimeout
	}
	if cfg.MaxSyncRetries <= 0 {
		cfg.MaxSyncRetries = defaults.MaxSyncRetries
	}
	if cfg.RetryBackoff <= 0 {
		cfg.RetryBackoff = defaults.RetryBackoff
	}
	return cfg
}

func (c Config) validate() error {
	if strings.TrimSpace(c.MemoryPath) == "" {
		return fmt.Errorf("memory path required")
	}
	if strings.TrimSpace(c.SQLitePath) == "" {
		return fmt.Errorf("sqlite path required")
	}
	if c.SyncInterval <= 0 {
		return fmt.Errorf("sync interval must be positive")
	}
	if c.SyncTimeout <= 0 {
		return fmt.Errorf("sync timeout must be positive")
	}
	if c.MaxSyncRetries < 0 {
		return fmt.Errorf("sync retries must be non-negative")
	}
	if c.RetryBackoff <= 0 {
		return fmt.Errorf("retry backoff must be positive")
	}
	return nil
}
