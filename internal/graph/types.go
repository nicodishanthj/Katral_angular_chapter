// File path: internal/graph/types.go
package graph

import "context"

// NeighborKind enumerates the categories of graph relationships surfaced by the
// dependency service.
type NeighborKind string

const (
	NeighborKindDependency NeighborKind = "dependency"
	NeighborKindImpact     NeighborKind = "impact"
	NeighborKindRelated    NeighborKind = "related"
)

// Neighbor describes a related program discovered via graph traversal. The
// Chain field captures the traversal path from the source program to the
// neighbor (inclusive of both endpoints).
type Neighbor struct {
	Program  string       `json:"program"`
	Name     string       `json:"name,omitempty"`
	Source   string       `json:"source,omitempty"`
	Distance int          `json:"distance,omitempty"`
	Weight   float64      `json:"weight,omitempty"`
	Chain    []string     `json:"chain,omitempty"`
	Kind     NeighborKind `json:"kind,omitempty"`
}

// DependencyService exposes read-only traversal helpers for enriching search
// results with graph awareness.
type DependencyService interface {
	Dependencies(ctx context.Context, program string, depth int) ([]Neighbor, error)
	Impacts(ctx context.Context, program string, depth int) ([]Neighbor, error)
	Related(ctx context.Context, program string, limit int) ([]Neighbor, error)
}

type noopDependencyService struct{}

// NoopDependencyService returns a DependencyService implementation that always
// yields empty traversals. It is useful when a graph backend is unavailable.
func NoopDependencyService() DependencyService { return noopDependencyService{} }

func (noopDependencyService) Dependencies(context.Context, string, int) ([]Neighbor, error) {
	return nil, nil
}

func (noopDependencyService) Impacts(context.Context, string, int) ([]Neighbor, error) {
	return nil, nil
}

func (noopDependencyService) Related(context.Context, string, int) ([]Neighbor, error) {
	return nil, nil
}

// Client defines the minimal operations required for interacting with a
// program knowledge graph backend.
type Client interface {
	// Available reports whether the underlying backend is reachable and ready
	// to accept queries.
	Available() bool
	// EnsureSchema guarantees that the required node tables and relationship
	// types exist in the backing store.
	EnsureSchema(ctx context.Context) error
	// InsertProgram upserts a COBOL program node.
	InsertProgram(ctx context.Context, program Program) error
	// InsertCall upserts a CALLS relationship between two programs.
	InsertCall(ctx context.Context, call Call) error
	// InsertParagraph upserts a paragraph node and attaches it to its program.
	InsertParagraph(ctx context.Context, paragraph Paragraph) error
	// InsertDataFlow upserts a DATA_FLOW relationship.
	InsertDataFlow(ctx context.Context, flow DataFlow) error
	// Close releases resources associated with the client.
	Close() error
}

// Program describes a COBOL program entry in the knowledge graph.
type Program struct {
	ID           string
	Name         string
	SourcePath   string
	Summary      string
	Technologies []string
	Inputs       []string
	Outputs      []string
	Paragraphs   []string
}

// Call represents an invocation edge between two programs.
type Call struct {
	From        string
	To          string
	Type        string
	Occurrences int
	Notes       string
}

// Paragraph captures a COBOL paragraph and its relationship to a program.
type Paragraph struct {
	ID        string
	ProgramID string
	Name      string
	Summary   string
	Logic     []string
}

// DataFlow represents a flow of data or control between two programs.
type DataFlow struct {
	From        string
	To          string
	Name        string
	Kind        string
	Description string
}
