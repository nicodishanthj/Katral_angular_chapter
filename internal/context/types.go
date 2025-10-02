// File path: internal/context/types.go
package context

import (
	"context"
	"time"

	"github.com/nicodishanthj/Katral_phase1/internal/graph"
	"github.com/nicodishanthj/Katral_phase1/internal/kb"
	"github.com/nicodishanthj/Katral_phase1/internal/metadata"
	"github.com/nicodishanthj/Katral_phase1/internal/vector"
)

// Embedder describes the minimal contract needed to generate vectors for
// queries against a vector store.
type Embedder interface {
	Embed(ctx context.Context, input []string) ([][]float32, error)
}

// DocumentService exposes document level search primitives consumed by the
// context builder.
type DocumentService interface {
	// MatchDocuments returns ranked document matches for a free-form query.
	MatchDocuments(query string, limit int) []DocumentMatch
	// ProgramDoc returns an aggregated program document view when available.
	ProgramDoc(ctx context.Context, projectID, program string, embedder Embedder, store vector.Store) (kb.ProgramDoc, bool)
	// ProjectDoc returns the project overview synthesized from stored docs.
	ProjectDoc(ctx context.Context, projectID string, embedder Embedder, store vector.Store) kb.ProjectDoc
}

// GraphContext captures the graph-derived metadata associated with a program.
type GraphContext struct {
	Program      string             `json:"program"`
	Dependencies []kb.GraphNeighbor `json:"dependencies,omitempty"`
	Impacts      []kb.GraphNeighbor `json:"impacts,omitempty"`
	Related      []kb.GraphNeighbor `json:"related,omitempty"`
	Signal       float64            `json:"signal,omitempty"`
}

// DocumentMatch represents a scored document returned from the document
// service.
type DocumentMatch struct {
	Doc   kb.Doc
	Score float64
}

// Snippet represents a trimmed piece of context surfaced to downstream
// consumers.
type Snippet struct {
	ID       string                  `json:"id"`
	Program  string                  `json:"program,omitempty"`
	Source   string                  `json:"source,omitempty"`
	Summary  string                  `json:"summary,omitempty"`
	Content  string                  `json:"content,omitempty"`
	Score    float64                 `json:"score,omitempty"`
	Origin   string                  `json:"origin,omitempty"`
	Metadata *metadata.ProgramRecord `json:"metadata,omitempty"`
	Graph    GraphContext            `json:"graph,omitempty"`
}

// GoalRequest controls how conversational context is assembled for an
// arbitrary query.
type GoalRequest struct {
	Query      string
	Limit      int
	ProjectID  string
	Collection string
	Embedder   Embedder
	UseVector  *bool
}

// GoalResult captures the ranked context produced for a query.
type GoalResult struct {
	Query      string
	Collection string
	UsedVector bool
	Snippets   []Snippet
}

// ProgramRequest controls how program-centric context is produced.
type ProgramRequest struct {
	Program   string
	ProjectID string
	Embedder  Embedder
}

// ProgramResult aggregates metadata for a single program.
type ProgramResult struct {
	Program  string
	Document *kb.ProgramDoc
	Metadata *metadata.ProgramRecord
	Graph    GraphContext
}

// Config controls the behaviour of the context builder.
type Config struct {
	MaxSnippets       int
	MaxSnippetRunes   int
	VectorLimit       int
	VectorWeight      float64
	DocumentWeight    float64
	MetadataWeight    float64
	GraphWeight       float64
	MinScore          float64
	GraphDepth        int
	GraphRelatedLimit int
	GraphCacheTTL     time.Duration
	MetadataCacheTTL  time.Duration
	VectorCacheTTL    time.Duration
}

// DefaultConfig returns the baseline configuration balancing recall and
// latency for context assembly.
func DefaultConfig() Config {
	return Config{
		MaxSnippets:       5,
		MaxSnippetRunes:   1200,
		VectorLimit:       10,
		VectorWeight:      1.0,
		DocumentWeight:    0.75,
		MetadataWeight:    0.25,
		GraphWeight:       0.3,
		MinScore:          0,
		GraphDepth:        3,
		GraphRelatedLimit: 10,
		GraphCacheTTL:     5 * time.Minute,
		MetadataCacheTTL:  2 * time.Minute,
		VectorCacheTTL:    2 * time.Minute,
	}
}

// Builder orchestrates the graph, vector, metadata, and document services to
// assemble contextual snippets.
type Builder interface {
	BuildGoalContext(ctx context.Context, req GoalRequest) (GoalResult, error)
	BuildProgramContext(ctx context.Context, req ProgramRequest) (ProgramResult, error)
	GraphDependencies() graph.DependencyService
}
