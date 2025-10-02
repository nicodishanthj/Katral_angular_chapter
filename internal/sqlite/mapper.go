// File path: internal/sqlite/mapper.go
package sqlite

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"strings"

	"github.com/jmoiron/sqlx"
	"github.com/nicodishanthj/Katral_phase1/internal/kb"
)

var relationshipKinds = map[string]func(doc kb.Doc) []string{
	"input":     func(doc kb.Doc) []string { return doc.Inputs },
	"output":    func(doc kb.Doc) []string { return doc.Outputs },
	"call":      func(doc kb.Doc) []string { return doc.Calls },
	"paragraph": func(doc kb.Doc) []string { return doc.Paragraphs },
	"logic":     func(doc kb.Doc) []string { return doc.Logic },
}

// PersistDoc normalises and stores a kb.Doc within the relational schema.
func (s *Store) PersistDoc(ctx context.Context, projectID string, doc kb.Doc) error {
	if s == nil || s.db == nil {
		return errors.New("sqlite store not initialised")
	}
	if strings.TrimSpace(doc.Program) == "" {
		return nil
	}
	return withTx(ctx, s.db, func(tx *sqlx.Tx) error {
		programID, err := upsertProgram(ctx, tx, projectID, doc)
		if err != nil {
			return err
		}
		if err := syncTechnologies(ctx, tx, programID, doc.Technologies); err != nil {
			return err
		}
		if err := syncRelationships(ctx, tx, programID, doc); err != nil {
			return err
		}
		if _, err := upsertFile(ctx, tx, programID, doc); err != nil {
			return err
		}
		return recordAudit(ctx, tx, projectID, sql.NullInt64{Int64: programID, Valid: true}, "doc_persisted", doc.ID)
	})
}

func upsertProgram(ctx context.Context, tx *sqlx.Tx, projectID string, doc kb.Doc) (int64, error) {
	query := `INSERT INTO programs(project_id, name, source_path, summary)
                VALUES(?, ?, ?, ?)
                ON CONFLICT(project_id, name) DO UPDATE SET
                        source_path = excluded.source_path,
                        summary = CASE WHEN excluded.summary != '' THEN excluded.summary ELSE programs.summary END,
                        updated_at = CURRENT_TIMESTAMP`
	if _, err := tx.ExecContext(ctx, query, projectID, doc.Program, doc.SourcePath, doc.Summary); err != nil {
		return 0, fmt.Errorf("upsert program: %w", err)
	}
	var programID int64
	if err := tx.GetContext(ctx, &programID, `SELECT id FROM programs WHERE project_id = ? AND name = ?`, projectID, doc.Program); err != nil {
		return 0, fmt.Errorf("load program: %w", err)
	}
	return programID, nil
}

func syncTechnologies(ctx context.Context, tx *sqlx.Tx, programID int64, technologies []string) error {
	if technologies == nil {
		return nil
	}
	if _, err := tx.ExecContext(ctx, `DELETE FROM program_technologies WHERE program_id = ?`, programID); err != nil {
		return fmt.Errorf("clear program technologies: %w", err)
	}
	for _, tech := range technologies {
		trimmed := strings.TrimSpace(tech)
		if trimmed == "" {
			continue
		}
		var techID int64
		err := tx.GetContext(ctx, &techID, `SELECT id FROM technologies WHERE name = ?`, trimmed)
		if errors.Is(err, sql.ErrNoRows) {
			res, ierr := tx.ExecContext(ctx, `INSERT INTO technologies(name) VALUES(?)`, trimmed)
			if ierr != nil {
				return fmt.Errorf("insert technology: %w", ierr)
			}
			techID, ierr = res.LastInsertId()
			if ierr != nil {
				return fmt.Errorf("technology id: %w", ierr)
			}
			err = nil
		}
		if err != nil {
			return fmt.Errorf("load technology: %w", err)
		}
		if _, err := tx.ExecContext(ctx, `INSERT OR IGNORE INTO program_technologies(program_id, technology_id) VALUES(?, ?)`, programID, techID); err != nil {
			return fmt.Errorf("link technology: %w", err)
		}
	}
	return nil
}

func syncRelationships(ctx context.Context, tx *sqlx.Tx, programID int64, doc kb.Doc) error {
	for kind, extractor := range relationshipKinds {
		values := extractor(doc)
		if values == nil {
			continue
		}
		if _, err := tx.ExecContext(ctx, `DELETE FROM relationships WHERE program_id = ? AND kind = ?`, programID, kind); err != nil {
			return fmt.Errorf("clear relationships %s: %w", kind, err)
		}
		for idx, value := range values {
			trimmed := strings.TrimSpace(value)
			if trimmed == "" {
				continue
			}
			if _, err := tx.ExecContext(ctx, `INSERT INTO relationships(program_id, kind, target, sequence) VALUES(?, ?, ?, ?)`, programID, kind, trimmed, idx); err != nil {
				return fmt.Errorf("insert relationship %s: %w", kind, err)
			}
		}
	}
	return nil
}

func upsertFile(ctx context.Context, tx *sqlx.Tx, programID int64, doc kb.Doc) (int64, error) {
	type existingFile struct {
		ID          int64  `db:"id"`
		Fingerprint string `db:"fingerprint"`
	}
	var existing existingFile
	err := tx.GetContext(ctx, &existing, `SELECT id, fingerprint FROM files WHERE doc_id = ?`, doc.ID)
	if errors.Is(err, sql.ErrNoRows) {
		res, ierr := tx.ExecContext(ctx, `INSERT INTO files(program_id, doc_id, chunk, doc_type, content, summary, fingerprint, created_at, updated_at)
                        VALUES(?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)`,
			programID, doc.ID, doc.Chunk, doc.Type, doc.Content, doc.Summary, doc.Fingerprint)
		if ierr != nil {
			return 0, fmt.Errorf("insert file: %w", ierr)
		}
		fileID, ierr := res.LastInsertId()
		if ierr != nil {
			return 0, fmt.Errorf("file id: %w", ierr)
		}
		if doc.Fingerprint != "" {
			if err := insertVersion(ctx, tx, fileID, doc.Fingerprint, doc.PreviousFingerprint); err != nil {
				return 0, err
			}
		}
		return fileID, nil
	}
	if err != nil {
		return 0, fmt.Errorf("load file: %w", err)
	}
	if _, err := tx.ExecContext(ctx, `UPDATE files SET program_id = ?, chunk = ?, doc_type = ?, content = ?, summary = ?, fingerprint = ?, updated_at = CURRENT_TIMESTAMP WHERE id = ?`,
		programID, doc.Chunk, doc.Type, doc.Content, doc.Summary, doc.Fingerprint, existing.ID); err != nil {
		return 0, fmt.Errorf("update file: %w", err)
	}
	if doc.Fingerprint != "" && doc.Fingerprint != existing.Fingerprint {
		if err := insertVersion(ctx, tx, existing.ID, doc.Fingerprint, existing.Fingerprint); err != nil {
			return 0, err
		}
	}
	return existing.ID, nil
}

func insertVersion(ctx context.Context, tx *sqlx.Tx, fileID int64, fingerprint, previous string) error {
	if _, err := tx.ExecContext(ctx, `INSERT INTO versions(file_id, fingerprint, previous_fingerprint) VALUES(?, ?, ?)`, fileID, fingerprint, nullIfEmpty(previous)); err != nil {
		return fmt.Errorf("insert version: %w", err)
	}
	return nil
}

func recordAudit(ctx context.Context, tx *sqlx.Tx, projectID string, programID sql.NullInt64, action, detail string) error {
	if _, err := tx.ExecContext(ctx, `INSERT INTO audit(project_id, program_id, action, detail) VALUES(?, ?, ?, ?)`,
		projectID, nullIfInvalid(programID), action, detail); err != nil {
		return fmt.Errorf("insert audit: %w", err)
	}
	return nil
}

func withTx(ctx context.Context, db *sqlx.DB, fn func(*sqlx.Tx) error) error {
	tx, err := db.BeginTxx(ctx, nil)
	if err != nil {
		return err
	}
	if err := fn(tx); err != nil {
		tx.Rollback()
		return err
	}
	return tx.Commit()
}

func nullIfEmpty(value string) interface{} {
	if strings.TrimSpace(value) == "" {
		return nil
	}
	return value
}

func nullIfInvalid(v sql.NullInt64) interface{} {
	if !v.Valid {
		return nil
	}
	return v.Int64
}
