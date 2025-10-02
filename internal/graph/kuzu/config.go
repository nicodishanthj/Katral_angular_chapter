// File path: internal/graph/kuzu/config.go
package kuzu

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"
)

// Config captures connection options for the Kuzu graph backend.
type Config struct {
	Endpoint       string        `json:"endpoint"`
	Database       string        `json:"database"`
	Username       string        `json:"username"`
	Password       string        `json:"password"`
	MaxConnections int           `json:"max_connections"`
	Timeout        time.Duration `json:"-"`
	TimeoutString  string        `json:"timeout"`

	HTTPMaxIdleConns       int           `json:"http_max_idle_conns"`
	HTTPMaxIdlePerHost     int           `json:"http_max_idle_per_host"`
	HTTPMaxConnsPerHost    int           `json:"http_max_conns_per_host"`
	HTTPIdleConnTimeout    time.Duration `json:"-"`
	HTTPIdleConnTimeoutStr string        `json:"http_idle_conn_timeout"`
}

// Merge overlays non-zero values from the override into the receiver.
func (c Config) Merge(override Config) Config {
	result := c
	if strings.TrimSpace(override.Endpoint) != "" {
		result.Endpoint = strings.TrimSpace(override.Endpoint)
	}
	if strings.TrimSpace(override.Database) != "" {
		result.Database = strings.TrimSpace(override.Database)
	}
	if strings.TrimSpace(override.Username) != "" {
		result.Username = strings.TrimSpace(override.Username)
	}
	if strings.TrimSpace(override.Password) != "" {
		result.Password = override.Password
	}
	if override.MaxConnections > 0 {
		result.MaxConnections = override.MaxConnections
	}
	if override.Timeout > 0 {
		result.Timeout = override.Timeout
	}
	if strings.TrimSpace(override.TimeoutString) != "" {
		result.TimeoutString = strings.TrimSpace(override.TimeoutString)
	}
	if override.HTTPMaxIdleConns > 0 {
		result.HTTPMaxIdleConns = override.HTTPMaxIdleConns
	}
	if override.HTTPMaxIdlePerHost > 0 {
		result.HTTPMaxIdlePerHost = override.HTTPMaxIdlePerHost
	}
	if override.HTTPMaxConnsPerHost > 0 {
		result.HTTPMaxConnsPerHost = override.HTTPMaxConnsPerHost
	}
	if override.HTTPIdleConnTimeout > 0 {
		result.HTTPIdleConnTimeout = override.HTTPIdleConnTimeout
	}
	if strings.TrimSpace(override.HTTPIdleConnTimeoutStr) != "" {
		result.HTTPIdleConnTimeoutStr = strings.TrimSpace(override.HTTPIdleConnTimeoutStr)
	}
	return result
}

// LoadConfig reads configuration from the KUZU_CONFIG_FILE if present and then
// applies environment variable overrides.
func LoadConfig() (Config, error) {
	cfg := Config{}
	if path := strings.TrimSpace(os.Getenv("KUZU_CONFIG_FILE")); path != "" {
		fileCfg, err := loadFromFile(path)
		if err != nil {
			return Config{}, err
		}
		cfg = cfg.Merge(fileCfg)
	}
	envCfg, err := loadFromEnv()
	if err != nil {
		return Config{}, err
	}
	cfg = cfg.Merge(envCfg)
	cfg.applyDefaults()
	return cfg, nil
}

func (c *Config) applyDefaults() {
	if strings.TrimSpace(c.Endpoint) == "" {
		c.Endpoint = "http://localhost:8001"
	}
	if strings.TrimSpace(c.Database) == "" {
		c.Database = "main"
	}
	if c.MaxConnections <= 0 {
		c.MaxConnections = 4
	}
	if c.Timeout <= 0 {
		if c.TimeoutString != "" {
			if parsed, err := time.ParseDuration(c.TimeoutString); err == nil {
				c.Timeout = parsed
			}
		}
		if c.Timeout <= 0 {
			c.Timeout = 5 * time.Second
		}
	}
	if c.HTTPMaxIdleConns <= 0 {
		c.HTTPMaxIdleConns = c.MaxConnections * 2
		if c.HTTPMaxIdleConns <= 0 {
			c.HTTPMaxIdleConns = 8
		}
	}
	if c.HTTPMaxIdlePerHost <= 0 {
		c.HTTPMaxIdlePerHost = c.MaxConnections
		if c.HTTPMaxIdlePerHost <= 0 {
			c.HTTPMaxIdlePerHost = 4
		}
	}
	if c.HTTPIdleConnTimeout <= 0 {
		if c.HTTPIdleConnTimeoutStr != "" {
			if parsed, err := time.ParseDuration(c.HTTPIdleConnTimeoutStr); err == nil {
				c.HTTPIdleConnTimeout = parsed
			}
		}
		if c.HTTPIdleConnTimeout <= 0 {
			c.HTTPIdleConnTimeout = 90 * time.Second
		}
	}
}

func (c Config) Enabled() bool {
	return strings.TrimSpace(c.Endpoint) != ""
}

func loadFromFile(path string) (Config, error) {
	data, err := os.ReadFile(filepath.Clean(path))
	if err != nil {
		return Config{}, fmt.Errorf("read kuzu config: %w", err)
	}
	var fileCfg Config
	if err := json.Unmarshal(data, &fileCfg); err != nil {
		return Config{}, fmt.Errorf("parse kuzu config: %w", err)
	}
	return fileCfg, nil
}

func loadFromEnv() (Config, error) {
	cfg := Config{}
	if endpoint := strings.TrimSpace(os.Getenv("KUZU_ENDPOINT")); endpoint != "" {
		cfg.Endpoint = endpoint
	}
	if database := strings.TrimSpace(os.Getenv("KUZU_DATABASE")); database != "" {
		cfg.Database = database
	}
	if username := strings.TrimSpace(os.Getenv("KUZU_USERNAME")); username != "" {
		cfg.Username = username
	}
	if password := os.Getenv("KUZU_PASSWORD"); password != "" {
		cfg.Password = password
	}
	if timeout := strings.TrimSpace(os.Getenv("KUZU_TIMEOUT")); timeout != "" {
		cfg.TimeoutString = timeout
		if parsed, err := time.ParseDuration(timeout); err == nil {
			cfg.Timeout = parsed
		}
	}
	if max := strings.TrimSpace(os.Getenv("KUZU_MAX_CONNECTIONS")); max != "" {
		value, err := strconv.Atoi(max)
		if err != nil {
			return Config{}, fmt.Errorf("parse KUZU_MAX_CONNECTIONS: %w", err)
		}
		if value > 0 {
			cfg.MaxConnections = value
		}
	}
	if maxIdle := strings.TrimSpace(os.Getenv("KUZU_HTTP_MAX_IDLE_CONNS")); maxIdle != "" {
		value, err := strconv.Atoi(maxIdle)
		if err != nil {
			return Config{}, fmt.Errorf("parse KUZU_HTTP_MAX_IDLE_CONNS: %w", err)
		}
		if value > 0 {
			cfg.HTTPMaxIdleConns = value
		}
	}
	if maxIdleHost := strings.TrimSpace(os.Getenv("KUZU_HTTP_MAX_IDLE_PER_HOST")); maxIdleHost != "" {
		value, err := strconv.Atoi(maxIdleHost)
		if err != nil {
			return Config{}, fmt.Errorf("parse KUZU_HTTP_MAX_IDLE_PER_HOST: %w", err)
		}
		if value > 0 {
			cfg.HTTPMaxIdlePerHost = value
		}
	}
	if maxConns := strings.TrimSpace(os.Getenv("KUZU_HTTP_MAX_CONNS_PER_HOST")); maxConns != "" {
		value, err := strconv.Atoi(maxConns)
		if err != nil {
			return Config{}, fmt.Errorf("parse KUZU_HTTP_MAX_CONNS_PER_HOST: %w", err)
		}
		if value > 0 {
			cfg.HTTPMaxConnsPerHost = value
		}
	}
	if idleTimeout := strings.TrimSpace(os.Getenv("KUZU_HTTP_IDLE_CONN_TIMEOUT")); idleTimeout != "" {
		cfg.HTTPIdleConnTimeoutStr = idleTimeout
		if parsed, err := time.ParseDuration(idleTimeout); err == nil {
			cfg.HTTPIdleConnTimeout = parsed
		}
	}
	return cfg, nil
}
