// File path: internal/sqlite/store.go
package sqlite

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"path/filepath"
	"strings"
	"time"

	"github.com/jmoiron/sqlx"
)

// Store wraps a pooled sqlx.DB connection to the SQLite catalog.
type Store struct {
	db *sqlx.DB
}

// Open constructs a Store backed by the SQLite database at the provided path.
// The database schema is automatically migrated and seeded on first use.
func Open(path string) (*Store, error) {
	cfg, err := LoadConfig()
	if err != nil {
		return nil, err
	}
	if trimmed := strings.TrimSpace(path); trimmed != "" {
		cfg.Path = trimmed
	}
	return OpenWithConfig(cfg)
}

// OpenWithConfig constructs a Store using the provided configuration.
func OpenWithConfig(cfg Config) (*Store, error) {
	if strings.TrimSpace(cfg.Path) == "" {
		return nil, errors.New("sqlite path required")
	}
	abs, err := filepath.Abs(cfg.Path)
	if err != nil {
		return nil, fmt.Errorf("resolve sqlite path: %w", err)
	}
	busy := int(cfg.BusyTimeout / time.Millisecond)
	if busy <= 0 {
		busy = 5000
	}
	dsn := fmt.Sprintf("file:%s?_pragma=busy_timeout(%d)&_pragma=foreign_keys(1)", abs, busy)
	db, err := sqlx.Open("sqlite", dsn)
	if err != nil {
		return nil, fmt.Errorf("open sqlite: %w", err)
	}
	db.SetMaxOpenConns(cfg.MaxOpenConns)
	db.SetMaxIdleConns(cfg.MaxIdleConns)
	db.SetConnMaxLifetime(cfg.ConnMaxLifetime)
	db.SetConnMaxIdleTime(cfg.ConnMaxIdleTime)

	pingTimeout := cfg.BusyTimeout
	if pingTimeout <= 0 {
		pingTimeout = 5 * time.Second
	}
	ctx, cancel := context.WithTimeout(context.Background(), pingTimeout)
	defer cancel()
	if err := db.PingContext(ctx); err != nil {
		db.Close()
		return nil, fmt.Errorf("ping sqlite: %w", err)
	}

	store := &Store{db: db}
	if err := store.migrate(context.Background()); err != nil {
		db.Close()
		return nil, err
	}
	return store, nil
}

// Close releases the underlying database resources.
func (s *Store) Close() error {
	if s == nil || s.db == nil {
		return nil
	}
	return s.db.Close()
}

// DB exposes the underlying sqlx.DB for advanced callers.
func (s *Store) DB() *sqlx.DB {
	if s == nil {
		return nil
	}
	return s.db
}

func (s *Store) migrate(ctx context.Context) error {
	if s == nil || s.db == nil {
		return errors.New("sqlite store not initialised")
	}
	tx, err := s.db.BeginTxx(ctx, &sql.TxOptions{})
	if err != nil {
		return fmt.Errorf("begin migration: %w", err)
	}
	for i, stmt := range schemaStatements {
		if _, err := tx.ExecContext(ctx, stmt); err != nil {
			tx.Rollback()
			return fmt.Errorf("execute schema statement %d: %w", i+1, err)
		}
	}
	if err := tx.Commit(); err != nil {
		return fmt.Errorf("commit migration: %w", err)
	}
	return nil
}

var schemaStatements = []string{
	`PRAGMA journal_mode = WAL;`,
	`PRAGMA foreign_keys = ON;`,
	`CREATE TABLE IF NOT EXISTS programs (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                project_id TEXT NOT NULL,
                name TEXT NOT NULL,
                source_path TEXT,
                summary TEXT,
                created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
                updated_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
                UNIQUE(project_id, name)
        );`,
	`CREATE TABLE IF NOT EXISTS files (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                program_id INTEGER NOT NULL,
                doc_id TEXT NOT NULL,
                chunk INTEGER,
                doc_type TEXT,
                content TEXT,
                summary TEXT,
                fingerprint TEXT,
                created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
                updated_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
                FOREIGN KEY(program_id) REFERENCES programs(id) ON DELETE CASCADE,
                UNIQUE(doc_id)
        );`,
	`CREATE TABLE IF NOT EXISTS technologies (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                name TEXT NOT NULL UNIQUE
        );`,
	`CREATE TABLE IF NOT EXISTS program_technologies (
                program_id INTEGER NOT NULL,
                technology_id INTEGER NOT NULL,
                PRIMARY KEY (program_id, technology_id),
                FOREIGN KEY(program_id) REFERENCES programs(id) ON DELETE CASCADE,
                FOREIGN KEY(technology_id) REFERENCES technologies(id) ON DELETE CASCADE
        );`,
	`CREATE TABLE IF NOT EXISTS relationships (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                program_id INTEGER NOT NULL,
                kind TEXT NOT NULL,
                target TEXT NOT NULL,
                sequence INTEGER NOT NULL DEFAULT 0,
                created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
                FOREIGN KEY(program_id) REFERENCES programs(id) ON DELETE CASCADE
        );`,
	`CREATE TABLE IF NOT EXISTS versions (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                file_id INTEGER NOT NULL,
                fingerprint TEXT NOT NULL,
                previous_fingerprint TEXT,
                created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
                FOREIGN KEY(file_id) REFERENCES files(id) ON DELETE CASCADE
        );`,
	`CREATE TABLE IF NOT EXISTS audit (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                project_id TEXT,
                program_id INTEGER,
                action TEXT NOT NULL,
                detail TEXT,
                created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
                FOREIGN KEY(program_id) REFERENCES programs(id) ON DELETE SET NULL
        );`,
	`CREATE INDEX IF NOT EXISTS idx_programs_project_name ON programs(project_id, name);`,
	`CREATE INDEX IF NOT EXISTS idx_programs_project_updated ON programs(project_id, updated_at);`,
	`CREATE INDEX IF NOT EXISTS idx_files_program ON files(program_id);`,
	`CREATE INDEX IF NOT EXISTS idx_files_program_doc_type ON files(program_id, doc_type);`,
	`CREATE INDEX IF NOT EXISTS idx_files_updated ON files(updated_at);`,
	`CREATE INDEX IF NOT EXISTS idx_relationships_program_kind ON relationships(program_id, kind);`,
	`CREATE INDEX IF NOT EXISTS idx_relationships_target_kind ON relationships(target, kind);`,
	`CREATE INDEX IF NOT EXISTS idx_versions_file_created ON versions(file_id, created_at);`,
	`CREATE VIEW IF NOT EXISTS program_doc_stats AS
                SELECT
                        p.id AS program_id,
                        p.project_id,
                        COUNT(f.id) AS document_count,
                        COUNT(DISTINCT f.doc_type) AS doc_type_count,
                        MAX(f.updated_at) AS last_document_update
                FROM programs p
                LEFT JOIN files f ON f.program_id = p.id
                GROUP BY p.id;`,
	`CREATE VIEW IF NOT EXISTS technology_usage_view AS
                SELECT
                        pt.program_id,
                        p.project_id,
                        t.name AS technology
                FROM program_technologies pt
                INNER JOIN programs p ON p.id = pt.program_id
                INNER JOIN technologies t ON t.id = pt.technology_id;`,
	`CREATE VIEW IF NOT EXISTS technology_dependency_view AS
                SELECT
                        tuv.project_id,
                        tuv.program_id,
                        tuv.technology,
                        COALESCE(rel.total_relationships, 0) AS dependency_count
                FROM technology_usage_view tuv
                LEFT JOIN (
                        SELECT program_id, COUNT(*) AS total_relationships
                        FROM relationships
                        GROUP BY program_id
                ) rel ON rel.program_id = tuv.program_id;`,
	`CREATE VIEW IF NOT EXISTS relationship_counts_view AS
                SELECT
                        p.project_id,
                        r.program_id,
                        p.name AS program,
                        r.kind,
                        r.target,
                        r.sequence,
                        r.created_at,
                        COUNT(*) OVER (PARTITION BY r.program_id, r.kind) AS count
                FROM relationships r
                INNER JOIN programs p ON p.id = r.program_id;`,
	`CREATE VIEW IF NOT EXISTS file_change_history AS
                SELECT
                        f.program_id,
                        f.doc_id,
                        f.doc_type,
                        v.fingerprint,
                        v.previous_fingerprint,
                        v.created_at
                FROM versions v
                INNER JOIN files f ON f.id = v.file_id;`,
	`INSERT INTO audit(project_id, action, detail)
        SELECT '', 'schema_created', 'initial schema loaded'
        WHERE NOT EXISTS (SELECT 1 FROM audit WHERE action = 'schema_created');`,
}
