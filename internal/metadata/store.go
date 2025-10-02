// File path: internal/metadata/store.go
package metadata

import (
	"context"
	"time"
)

// QueryOptions control how program metadata is filtered when listing from the
// catalog. All fields are optional unless stated otherwise.
type QueryOptions struct {
	ProjectID string

	Technologies []string
	DocTypes     []string

	NamePattern string

	UpdatedAfter  *time.Time
	UpdatedBefore *time.Time

	Limit  int
	Offset int
}

// ProgramRecord represents a single program row returned from the catalog with
// aggregated statistics derived from related tables.
type ProgramRecord struct {
	ID         int64     `json:"id"`
	ProjectID  string    `json:"project_id"`
	Name       string    `json:"name"`
	SourcePath string    `json:"source_path"`
	Summary    string    `json:"summary"`
	CreatedAt  time.Time `json:"created_at"`
	UpdatedAt  time.Time `json:"updated_at"`

	LastDocumentUpdate *time.Time `json:"last_document_update,omitempty"`
	DocumentCount      int        `json:"document_count"`
	DocTypeCount       int        `json:"doc_type_count"`
	Technologies       []string   `json:"technologies"`
}

// ProgramsPage represents a paginated response from the catalog.
type ProgramsPage struct {
	Programs []ProgramRecord `json:"programs"`
	Total    int             `json:"total"`
	Offset   int             `json:"offset"`
	Limit    int             `json:"limit"`
}

// TechnologyUsage represents aggregated program counts per technology.
type TechnologyUsage struct {
	Technology        string `json:"technology"`
	ProgramCount      int    `json:"program_count"`
	RelationshipCount int    `json:"relationship_count"`
}

// DependencyCount aggregates relationship counts per kind for a project or
// program.
type DependencyCount struct {
	ProgramID int64  `json:"program_id"`
	Kind      string `json:"kind"`
	Count     int    `json:"count"`
}

// ChangeEvent captures the historical modifications for a file in a program.
type ChangeEvent struct {
	ProgramID   int64     `json:"program_id"`
	DocID       string    `json:"doc_id"`
	DocType     string    `json:"doc_type"`
	Fingerprint string    `json:"fingerprint"`
	Previous    string    `json:"previous"`
	CreatedAt   time.Time `json:"created_at"`
}

// CrossReference represents a link between programs via the relationships
// table.
type CrossReference struct {
	ProgramID int64     `json:"program_id"`
	Program   string    `json:"program"`
	Kind      string    `json:"kind"`
	Target    string    `json:"target"`
	Sequence  int       `json:"sequence"`
	CreatedAt time.Time `json:"created_at"`
}

// CrossReferenceOptions configure cross reference lookups.
type CrossReferenceOptions struct {
	ProjectID string
	Target    string
	Kinds     []string
	Limit     int
	Offset    int
}

// ProgramUpsert represents a batch upsert payload for programs and associated
// technologies.
type ProgramUpsert struct {
	ProjectID    string
	Name         string
	SourcePath   string
	Summary      string
	Technologies []string
}

// FileUpsert represents a batch upsert payload for files/documents.
type FileUpsert struct {
	ProgramID   int64
	DocID       string
	Chunk       int
	DocType     string
	Content     string
	Summary     string
	Fingerprint string
}

// Store exposes metadata catalog operations backed by a persistent data store.
type Store interface {
	QueryPrograms(ctx context.Context, opts QueryOptions) (ProgramsPage, error)
	StreamPrograms(ctx context.Context, opts QueryOptions, fn func(ProgramRecord) error) error

	TechnologyUsage(ctx context.Context, projectID string) ([]TechnologyUsage, error)
	DependencyCounts(ctx context.Context, projectID string, programID *int64) ([]DependencyCount, error)
	ChangeHistory(ctx context.Context, programID int64, limit int) ([]ChangeEvent, error)
	CrossReferences(ctx context.Context, opts CrossReferenceOptions) ([]CrossReference, error)

	BatchUpsertPrograms(ctx context.Context, programs []ProgramUpsert) error
	BatchUpsertFiles(ctx context.Context, files []FileUpsert) error
}
