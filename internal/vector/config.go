// File path: internal/vector/config.go
package vector

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
	Host       string `json:"host"`
	Port       string `json:"port"`
	Scheme     string `json:"scheme"`
	Collection string `json:"collection"`
	APIKey     string `json:"api_key"`

	Timeout       time.Duration `json:"-"`
	TimeoutString string        `json:"timeout"`

	HTTPMaxIdleConns       int           `json:"http_max_idle_conns"`
	HTTPMaxIdlePerHost     int           `json:"http_max_idle_per_host"`
	HTTPMaxConnsPerHost    int           `json:"http_max_conns_per_host"`
	HTTPIdleConnTimeout    time.Duration `json:"-"`
	HTTPIdleConnTimeoutStr string        `json:"http_idle_conn_timeout"`
}

func (c Config) Merge(override Config) Config {
	result := c
	if strings.TrimSpace(override.Host) != "" {
		result.Host = strings.TrimSpace(override.Host)
	}
	if strings.TrimSpace(override.Port) != "" {
		result.Port = strings.TrimSpace(override.Port)
	}
	if strings.TrimSpace(override.Scheme) != "" {
		result.Scheme = strings.TrimSpace(override.Scheme)
	}
	if strings.TrimSpace(override.Collection) != "" {
		result.Collection = strings.TrimSpace(override.Collection)
	}
	if strings.TrimSpace(override.APIKey) != "" {
		result.APIKey = override.APIKey
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

func LoadConfig() (Config, error) {
	cfg := Config{}
	if path := strings.TrimSpace(os.Getenv("CHROMADB_CONFIG_FILE")); path != "" {
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
	if strings.TrimSpace(c.Host) == "" {
		c.Host = "localhost"
	}
	if strings.TrimSpace(c.Port) == "" {
		c.Port = "8000"
	}
	if strings.TrimSpace(c.Scheme) == "" {
		c.Scheme = "http"
	}
	if strings.TrimSpace(c.Collection) == "" {
		c.Collection = "cwa_docs"
	}
	if c.Timeout <= 0 {
		if c.TimeoutString != "" {
			if parsed, err := time.ParseDuration(c.TimeoutString); err == nil {
				c.Timeout = parsed
			}
		}
		if c.Timeout <= 0 {
			c.Timeout = 10 * time.Second
		}
	}
	if c.HTTPMaxIdleConns <= 0 {
		c.HTTPMaxIdleConns = 64
	}
	if c.HTTPMaxIdlePerHost <= 0 {
		c.HTTPMaxIdlePerHost = 16
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

func loadConfigFile(path string) (Config, error) {
	data, err := os.ReadFile(filepath.Clean(path))
	if err != nil {
		return Config{}, fmt.Errorf("read chromadb config: %w", err)
	}
	var cfg Config
	if err := json.Unmarshal(data, &cfg); err != nil {
		return Config{}, fmt.Errorf("parse chromadb config: %w", err)
	}
	return cfg, nil
}

func loadConfigEnv() (Config, error) {
	cfg := Config{}
	if host := strings.TrimSpace(os.Getenv("CHROMADB_HOST")); host != "" {
		cfg.Host = host
	}
	if port := strings.TrimSpace(os.Getenv("CHROMADB_PORT")); port != "" {
		cfg.Port = port
	}
	if scheme := strings.TrimSpace(os.Getenv("CHROMADB_SCHEME")); scheme != "" {
		cfg.Scheme = scheme
	}
	if collection := strings.TrimSpace(os.Getenv("CHROMADB_COLLECTION")); collection != "" {
		cfg.Collection = collection
	}
	if apiKey := strings.TrimSpace(os.Getenv("CHROMADB_API_KEY")); apiKey != "" {
		cfg.APIKey = apiKey
	}
	if timeout := strings.TrimSpace(os.Getenv("CHROMADB_TIMEOUT")); timeout != "" {
		cfg.TimeoutString = timeout
		if parsed, err := time.ParseDuration(timeout); err == nil {
			cfg.Timeout = parsed
		}
	}
	if maxIdle := strings.TrimSpace(os.Getenv("CHROMADB_HTTP_MAX_IDLE_CONNS")); maxIdle != "" {
		value, err := strconv.Atoi(maxIdle)
		if err != nil {
			return Config{}, fmt.Errorf("parse CHROMADB_HTTP_MAX_IDLE_CONNS: %w", err)
		}
		if value > 0 {
			cfg.HTTPMaxIdleConns = value
		}
	}
	if maxIdleHost := strings.TrimSpace(os.Getenv("CHROMADB_HTTP_MAX_IDLE_PER_HOST")); maxIdleHost != "" {
		value, err := strconv.Atoi(maxIdleHost)
		if err != nil {
			return Config{}, fmt.Errorf("parse CHROMADB_HTTP_MAX_IDLE_PER_HOST: %w", err)
		}
		if value > 0 {
			cfg.HTTPMaxIdlePerHost = value
		}
	}
	if maxConns := strings.TrimSpace(os.Getenv("CHROMADB_HTTP_MAX_CONNS_PER_HOST")); maxConns != "" {
		value, err := strconv.Atoi(maxConns)
		if err != nil {
			return Config{}, fmt.Errorf("parse CHROMADB_HTTP_MAX_CONNS_PER_HOST: %w", err)
		}
		if value > 0 {
			cfg.HTTPMaxConnsPerHost = value
		}
	}
	if idleTimeout := strings.TrimSpace(os.Getenv("CHROMADB_HTTP_IDLE_CONN_TIMEOUT")); idleTimeout != "" {
		cfg.HTTPIdleConnTimeoutStr = idleTimeout
		if parsed, err := time.ParseDuration(idleTimeout); err == nil {
			cfg.HTTPIdleConnTimeout = parsed
		}
	}
	return cfg, nil
}
