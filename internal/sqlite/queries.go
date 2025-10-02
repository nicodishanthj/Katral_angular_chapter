// File path: internal/sqlite/queries.go
package sqlite

import (
	"context"
	"fmt"
	"strings"

	"github.com/jmoiron/sqlx"
)

// ListPrograms returns all programs for a given project identifier.
func (s *Store) ListPrograms(ctx context.Context, projectID string) ([]Program, error) {
	if s == nil || s.db == nil {
		return nil, fmt.Errorf("sqlite store not initialised")
	}
	programs := []Program{}
	if err := s.db.SelectContext(ctx, &programs, `SELECT * FROM programs WHERE project_id = ? ORDER BY name`, projectID); err != nil {
		return nil, fmt.Errorf("select programs: %w", err)
	}
	return programs, nil
}

// ProgramByName retrieves a single program by project and name.
func (s *Store) ProgramByName(ctx context.Context, projectID, name string) (*Program, error) {
	if s == nil || s.db == nil {
		return nil, fmt.Errorf("sqlite store not initialised")
	}
	name = strings.TrimSpace(name)
	if name == "" {
		return nil, fmt.Errorf("program name required")
	}
	var program Program
	if err := s.db.GetContext(ctx, &program, `SELECT * FROM programs WHERE project_id = ? AND name = ?`, projectID, name); err != nil {
		return nil, err
	}
	return &program, nil
}

// FilesForProgram returns the persisted knowledge base documents for a program.
func (s *Store) FilesForProgram(ctx context.Context, programID int64) ([]File, error) {
	if s == nil || s.db == nil {
		return nil, fmt.Errorf("sqlite store not initialised")
	}
	files := []File{}
	if err := s.db.SelectContext(ctx, &files, `SELECT * FROM files WHERE program_id = ? ORDER BY chunk, id`, programID); err != nil {
		return nil, fmt.Errorf("select files: %w", err)
	}
	return files, nil
}

// RelationshipsForProgram returns metadata relationships filtered by kinds (if provided).
func (s *Store) RelationshipsForProgram(ctx context.Context, programID int64, kinds ...string) ([]Relationship, error) {
	if s == nil || s.db == nil {
		return nil, fmt.Errorf("sqlite store not initialised")
	}
	relationships := []Relationship{}
	if len(kinds) == 0 {
		if err := s.db.SelectContext(ctx, &relationships, `SELECT * FROM relationships WHERE program_id = ? ORDER BY kind, sequence`, programID); err != nil {
			return nil, fmt.Errorf("select relationships: %w", err)
		}
		return relationships, nil
	}
	query, args, err := sqlx.In(`SELECT * FROM relationships WHERE program_id = ? AND kind IN (?) ORDER BY kind, sequence`, programID, kinds)
	if err != nil {
		return nil, fmt.Errorf("build relationship query: %w", err)
	}
	query = s.db.Rebind(query)
	if err := s.db.SelectContext(ctx, &relationships, query, args...); err != nil {
		return nil, fmt.Errorf("select relationships: %w", err)
	}
	return relationships, nil
}

// TechnologiesForProgram lists associated technologies for a program.
func (s *Store) TechnologiesForProgram(ctx context.Context, programID int64) ([]string, error) {
	if s == nil || s.db == nil {
		return nil, fmt.Errorf("sqlite store not initialised")
	}
	technologies := []string{}
	if err := s.db.SelectContext(ctx, &technologies, `SELECT t.name FROM technologies t
                INNER JOIN program_technologies pt ON pt.technology_id = t.id
                WHERE pt.program_id = ?
                ORDER BY t.name`, programID); err != nil {
		return nil, fmt.Errorf("select technologies: %w", err)
	}
	return technologies, nil
}
