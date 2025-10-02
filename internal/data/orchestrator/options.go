// File path: internal/data/orchestrator/options.go
package orchestrator

import (
	"github.com/nicodishanthj/Katral_phase1/internal/graph"
	"github.com/nicodishanthj/Katral_phase1/internal/vector"
)

type Option func(*options)

type options struct {
	disableSync bool
	vector      vector.Store
	graph       graph.Client
}

// WithSyncDisabled prevents the orchestrator from starting the background
// synchronisation loop. Primarily used in tests.
func WithSyncDisabled() Option {
	return func(o *options) {
		o.disableSync = true
	}
}

// WithVectorStore injects a vector store implementation.
func WithVectorStore(store vector.Store) Option {
	return func(o *options) {
		o.vector = store
	}
}

// WithGraphClient injects a graph client implementation.
func WithGraphClient(client graph.Client) Option {
	return func(o *options) {
		o.graph = client
	}
}
