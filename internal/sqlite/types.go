// File path: internal/sqlite/types.go
package sqlite

import "time"

// Program represents a program metadata row.
type Program struct {
	ID         int64     `db:"id"`
	ProjectID  string    `db:"project_id"`
	Name       string    `db:"name"`
	SourcePath string    `db:"source_path"`
	Summary    string    `db:"summary"`
	CreatedAt  time.Time `db:"created_at"`
	UpdatedAt  time.Time `db:"updated_at"`
}

// File represents a stored knowledge base document chunk.
type File struct {
	ID          int64     `db:"id"`
	ProgramID   int64     `db:"program_id"`
	DocID       string    `db:"doc_id"`
	Chunk       int       `db:"chunk"`
	DocType     string    `db:"doc_type"`
	Content     string    `db:"content"`
	Summary     string    `db:"summary"`
	Fingerprint string    `db:"fingerprint"`
	CreatedAt   time.Time `db:"created_at"`
	UpdatedAt   time.Time `db:"updated_at"`
}

// Relationship represents a linked metadata entry (input, output, call, paragraph, logic).
type Relationship struct {
	ID        int64     `db:"id"`
	ProgramID int64     `db:"program_id"`
	Kind      string    `db:"kind"`
	Target    string    `db:"target"`
	Sequence  int       `db:"sequence"`
	CreatedAt time.Time `db:"created_at"`
}

// Version stores the historical fingerprint for a file.
type Version struct {
	ID                  int64     `db:"id"`
	FileID              int64     `db:"file_id"`
	Fingerprint         string    `db:"fingerprint"`
	PreviousFingerprint string    `db:"previous_fingerprint"`
	CreatedAt           time.Time `db:"created_at"`
}

// AuditRow represents an audit entry.
type AuditRow struct {
	ID        int64     `db:"id"`
	ProjectID string    `db:"project_id"`
	ProgramID *int64    `db:"program_id"`
	Action    string    `db:"action"`
	Detail    string    `db:"detail"`
	CreatedAt time.Time `db:"created_at"`
}
