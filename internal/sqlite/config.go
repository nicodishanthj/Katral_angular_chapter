// File path: internal/sqlite/config.go
package sqlite

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"
)

type Config struct {
	Path string `json:"path"`

	MaxOpenConns int `json:"max_open_conns"`
	MaxIdleConns int `json:"max_idle_conns"`

	ConnMaxLifetime       time.Duration `json:"-"`
	ConnMaxLifetimeString string        `json:"conn_max_lifetime"`

	ConnMaxIdleTime       time.Duration `json:"-"`
	ConnMaxIdleTimeString string        `json:"conn_max_idle_time"`

	BusyTimeout       time.Duration `json:"-"`
	BusyTimeoutString string        `json:"busy_timeout"`
}

func (c Config) Merge(override Config) Config {
	result := c
	if strings.TrimSpace(override.Path) != "" {
		result.Path = strings.TrimSpace(override.Path)
	}
	if override.MaxOpenConns > 0 {
		result.MaxOpenConns = override.MaxOpenConns
	}
	if override.MaxIdleConns > 0 {
		result.MaxIdleConns = override.MaxIdleConns
	}
	if override.ConnMaxLifetime > 0 {
		result.ConnMaxLifetime = override.ConnMaxLifetime
	}
	if strings.TrimSpace(override.ConnMaxLifetimeString) != "" {
		result.ConnMaxLifetimeString = strings.TrimSpace(override.ConnMaxLifetimeString)
	}
	if override.ConnMaxIdleTime > 0 {
		result.ConnMaxIdleTime = override.ConnMaxIdleTime
	}
	if strings.TrimSpace(override.ConnMaxIdleTimeString) != "" {
		result.ConnMaxIdleTimeString = strings.TrimSpace(override.ConnMaxIdleTimeString)
	}
	if override.BusyTimeout > 0 {
		result.BusyTimeout = override.BusyTimeout
	}
	if strings.TrimSpace(override.BusyTimeoutString) != "" {
		result.BusyTimeoutString = strings.TrimSpace(override.BusyTimeoutString)
	}
	return result
}

func LoadConfig() (Config, error) {
	cfg := Config{}
	if path := strings.TrimSpace(os.Getenv("SQLITE_CONFIG_FILE")); path != "" {
		fileCfg, err := loadConfigFile(path)
		if err != nil {
			return Config{}, err
		}
		cfg = cfg.Merge(fileCfg)
	}
	envCfg, err := loadConfigEnv()
	if err != nil {
		return Config{}, err
	}
	cfg = cfg.Merge(envCfg)
	cfg.applyDefaults()
	return cfg, nil
}

func (c *Config) applyDefaults() {
	if c.MaxOpenConns <= 0 {
		c.MaxOpenConns = 8
	}
	if c.MaxIdleConns <= 0 {
		c.MaxIdleConns = c.MaxOpenConns
	}
	if c.ConnMaxLifetime <= 0 {
		if c.ConnMaxLifetimeString != "" {
			if parsed, err := time.ParseDuration(c.ConnMaxLifetimeString); err == nil {
				c.ConnMaxLifetime = parsed
			}
		}
		if c.ConnMaxLifetime <= 0 {
			c.ConnMaxLifetime = 15 * time.Minute
		}
	}
	if c.ConnMaxIdleTime <= 0 {
		if c.ConnMaxIdleTimeString != "" {
			if parsed, err := time.ParseDuration(c.ConnMaxIdleTimeString); err == nil {
				c.ConnMaxIdleTime = parsed
			}
		}
		if c.ConnMaxIdleTime <= 0 {
			c.ConnMaxIdleTime = 5 * time.Minute
		}
	}
	if c.BusyTimeout <= 0 {
		if c.BusyTimeoutString != "" {
			if parsed, err := time.ParseDuration(c.BusyTimeoutString); err == nil {
				c.BusyTimeout = parsed
			}
		}
		if c.BusyTimeout <= 0 {
			c.BusyTimeout = 5 * time.Second
		}
	}
}

func loadConfigFile(path string) (Config, error) {
	data, err := os.ReadFile(filepath.Clean(path))
	if err != nil {
		return Config{}, fmt.Errorf("read sqlite config: %w", err)
	}
	var cfg Config
	if err := json.Unmarshal(data, &cfg); err != nil {
		return Config{}, fmt.Errorf("parse sqlite config: %w", err)
	}
	return cfg, nil
}

func loadConfigEnv() (Config, error) {
	cfg := Config{}
	if path := strings.TrimSpace(os.Getenv("SQLITE_PATH")); path != "" {
		cfg.Path = path
	}
	if openConns := strings.TrimSpace(os.Getenv("SQLITE_MAX_OPEN_CONNS")); openConns != "" {
		value, err := strconv.Atoi(openConns)
		if err != nil {
			return Config{}, fmt.Errorf("parse SQLITE_MAX_OPEN_CONNS: %w", err)
		}
		if value > 0 {
			cfg.MaxOpenConns = value
		}
	}
	if idleConns := strings.TrimSpace(os.Getenv("SQLITE_MAX_IDLE_CONNS")); idleConns != "" {
		value, err := strconv.Atoi(idleConns)
		if err != nil {
			return Config{}, fmt.Errorf("parse SQLITE_MAX_IDLE_CONNS: %w", err)
		}
		if value > 0 {
			cfg.MaxIdleConns = value
		}
	}
	if lifetime := strings.TrimSpace(os.Getenv("SQLITE_CONN_MAX_LIFETIME")); lifetime != "" {
		cfg.ConnMaxLifetimeString = lifetime
		if parsed, err := time.ParseDuration(lifetime); err == nil {
			cfg.ConnMaxLifetime = parsed
		}
	}
	if idle := strings.TrimSpace(os.Getenv("SQLITE_CONN_MAX_IDLE_TIME")); idle != "" {
		cfg.ConnMaxIdleTimeString = idle
		if parsed, err := time.ParseDuration(idle); err == nil {
			cfg.ConnMaxIdleTime = parsed
		}
	}
	if busy := strings.TrimSpace(os.Getenv("SQLITE_BUSY_TIMEOUT")); busy != "" {
		cfg.BusyTimeoutString = busy
		if parsed, err := time.ParseDuration(busy); err == nil {
			cfg.BusyTimeout = parsed
		}
	}
	return cfg, nil
}
