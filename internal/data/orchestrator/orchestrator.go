// File path: internal/data/orchestrator/orchestrator.go
package orchestrator

import (
	"context"
	"errors"
	"fmt"
	"os"
	"strings"

	"github.com/nicodishanthj/Katral_phase1/internal/graph"
	"github.com/nicodishanthj/Katral_phase1/internal/graph/kuzu"
	"github.com/nicodishanthj/Katral_phase1/internal/memory"
	"github.com/nicodishanthj/Katral_phase1/internal/metadata"
	"github.com/nicodishanthj/Katral_phase1/internal/sqlite"
	"github.com/nicodishanthj/Katral_phase1/internal/vector"
)

type closer interface {
	Close() error
}

// Orchestrator wires together the persistent stores that back the CWA server
// and exposes convenience accessors for the API layer.
type Orchestrator struct {
	cfg Config

	memoryStore *memory.Store
	catalog     *sqlite.Store
	metadata    metadata.Store
	vector      vector.Store
	graph       graph.Client

	syncDisabled bool

	closers []closer
}

// New constructs an orchestrator from the provided configuration and optional
// overrides.
func New(ctx context.Context, cfg Config, opts ...Option) (*Orchestrator, error) {
	cfg = applyDefaults(cfg)
	if err := cfg.validate(); err != nil {
		return nil, err
	}
	settings := options{}
	for _, opt := range opts {
		if opt != nil {
			opt(&settings)
		}
	}

	memStore, err := memory.NewStore(cfg.MemoryPath)
	if err != nil {
		return nil, fmt.Errorf("init memory store: %w", err)
	}
	catalog, err := sqlite.Open(cfg.SQLitePath)
	if err != nil {
		return nil, fmt.Errorf("init sqlite store: %w", err)
	}

	var vec vector.Store
	switch {
	case settings.vector != nil:
		vec = settings.vector
	case shouldEnableVector():
		client, err := vector.NewFromEnv(ctx)
		if err != nil {
			catalog.Close()
			return nil, fmt.Errorf("init vector client: %w", err)
		}
		vec = client
	}

	var graphClient graph.Client
	switch {
	case settings.graph != nil:
		graphClient = settings.graph
	case shouldEnableGraph():
		client, err := kuzu.NewFromEnv(ctx)
		if err != nil {
			catalog.Close()
			if client != nil {
				client.Close()
			}
			return nil, fmt.Errorf("init graph client: %w", err)
		}
		graphClient = client
	}

	orch := &Orchestrator{
		cfg:          cfg,
		memoryStore:  memStore,
		catalog:      catalog,
		metadata:     catalog,
		vector:       vec,
		graph:        graphClient,
		syncDisabled: settings.disableSync,
	}
	orch.closers = append(orch.closers, catalog)
	if graphClient != nil {
		orch.closers = append(orch.closers, graphClient)
	}
	return orch, nil
}

// Memory exposes the configured document store.
func (o *Orchestrator) Memory() *memory.Store {
	if o == nil {
		return nil
	}
	return o.memoryStore
}

// Metadata exposes the metadata catalog interface.
func (o *Orchestrator) Metadata() metadata.Store {
	if o == nil {
		return nil
	}
	return o.metadata
}

// Catalog is an alias for the metadata catalog, maintained for clarity at call
// sites.
func (o *Orchestrator) Catalog() metadata.Store {
	if o == nil {
		return nil
	}
	return o.metadata
}

// Vector exposes the optional vector store.
func (o *Orchestrator) Vector() vector.Store {
	if o == nil {
		return nil
	}
	return o.vector
}

// Graph exposes the optional graph client.
func (o *Orchestrator) Graph() graph.Client {
	if o == nil {
		return nil
	}
	return o.graph
}

// Close releases any resources associated with the orchestrator.
func (o *Orchestrator) Close() error {
	if o == nil {
		return nil
	}
	var err error
	for i := len(o.closers) - 1; i >= 0; i-- {
		closer := o.closers[i]
		if closer == nil {
			continue
		}
		if cerr := closer.Close(); cerr != nil {
			err = errors.Join(err, cerr)
		}
	}
	return err
}

func shouldEnableVector() bool {
	keys := []string{
		"CHROMADB_CONFIG_FILE",
		"CHROMADB_HOST",
		"CHROMADB_PORT",
		"CHROMADB_SCHEME",
		"CHROMADB_COLLECTION",
		"CHROMADB_API_KEY",
		"CHROMADB_TIMEOUT",
		"CHROMADB_HTTP_MAX_IDLE_CONNS",
		"CHROMADB_HTTP_MAX_IDLE_PER_HOST",
		"CHROMADB_HTTP_MAX_CONNS_PER_HOST",
		"CHROMADB_HTTP_IDLE_CONN_TIMEOUT",
	}
	for _, key := range keys {
		if value, ok := os.LookupEnv(key); ok && strings.TrimSpace(value) != "" {
			return true
		}
	}
	return false
}

func shouldEnableGraph() bool {
	keys := []string{
		"KUZU_CONFIG_FILE",
		"KUZU_ENDPOINT",
		"KUZU_DATABASE",
		"KUZU_USERNAME",
		"KUZU_PASSWORD",
		"KUZU_TIMEOUT",
		"KUZU_MAX_CONNECTIONS",
		"KUZU_HTTP_MAX_IDLE_CONNS",
		"KUZU_HTTP_MAX_IDLE_PER_HOST",
		"KUZU_HTTP_MAX_CONNS_PER_HOST",
		"KUZU_HTTP_IDLE_CONN_TIMEOUT",
	}
	for _, key := range keys {
		if value, ok := os.LookupEnv(key); ok && strings.TrimSpace(value) != "" {
			return true
		}
	}
	return false
}
