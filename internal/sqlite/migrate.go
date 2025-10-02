// File path: internal/sqlite/migrate.go
package sqlite

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"github.com/jmoiron/sqlx"
	"github.com/nicodishanthj/Katral_phase1/internal/kb"
	"github.com/nicodishanthj/Katral_phase1/internal/memory"
)

// SyncProjectFromMemory migrates a project's documents from the JSONL memory store into SQLite.
func (s *Store) SyncProjectFromMemory(ctx context.Context, projectID string, mem *memory.Store) error {
	if s == nil {
		return errors.New("sqlite store not initialised")
	}
	if mem == nil {
		return errors.New("memory store not provided")
	}
	docs, err := mem.AllDocs(ctx, projectID)
	if err != nil {
		return fmt.Errorf("load memory docs: %w", err)
	}
	for _, doc := range docs {
		if err := s.PersistDoc(ctx, projectID, doc); err != nil {
			return fmt.Errorf("persist doc %s: %w", doc.ID, err)
		}
	}
	return withTx(ctx, s.db, func(tx *sqlx.Tx) error {
		return recordAudit(ctx, tx, projectID, sql.NullInt64{}, "memory_migration", fmt.Sprintf("migrated %d docs", len(docs)))
	})
}

// PersistDocs stores the provided docs in a single transaction.
func (s *Store) PersistDocs(ctx context.Context, projectID string, docs []kb.Doc) error {
	if len(docs) == 0 {
		return nil
	}
	for _, doc := range docs {
		if err := s.PersistDoc(ctx, projectID, doc); err != nil {
			return err
		}
	}
	return nil
}
