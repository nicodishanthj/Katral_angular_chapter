// File path: internal/sqlite/metadata.go
package sqlite

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"sort"
	"strings"
	"time"

	"github.com/jmoiron/sqlx"

	"github.com/nicodishanthj/Katral_phase1/internal/metadata"
)

var (
	errNilStore = errors.New("sqlite store not initialised")
)

func (s *Store) ensureReady() error {
	if s == nil || s.db == nil {
		return errNilStore
	}
	return nil
}

// QueryPrograms implements metadata.Store by returning a filtered list of
// programs using the pre-computed catalog views for statistics.
func (s *Store) QueryPrograms(ctx context.Context, opts metadata.QueryOptions) (metadata.ProgramsPage, error) {
	if err := s.ensureReady(); err != nil {
		return metadata.ProgramsPage{}, err
	}
	if strings.TrimSpace(opts.ProjectID) == "" {
		return metadata.ProgramsPage{}, fmt.Errorf("project id required")
	}

	limit := opts.Limit
	if limit <= 0 {
		limit = 100
	}
	offset := opts.Offset
	if offset < 0 {
		offset = 0
	}

	filters := []string{"p.project_id = ?"}
	args := []interface{}{opts.ProjectID}

	if opts.NamePattern != "" {
		filters = append(filters, "p.name LIKE ?")
		args = append(args, opts.NamePattern)
	}
	if opts.UpdatedAfter != nil {
		filters = append(filters, "stats.last_document_update >= ?")
		args = append(args, opts.UpdatedAfter.UTC())
	}
	if opts.UpdatedBefore != nil {
		filters = append(filters, "stats.last_document_update <= ?")
		args = append(args, opts.UpdatedBefore.UTC())
	}
	if len(opts.DocTypes) > 0 {
		q, qArgs, err := sqlx.In("EXISTS (SELECT 1 FROM files f2 WHERE f2.program_id = p.id AND f2.doc_type IN (?))", opts.DocTypes)
		if err != nil {
			return metadata.ProgramsPage{}, fmt.Errorf("doc type filter: %w", err)
		}
		filters = append(filters, q)
		args = append(args, qArgs...)
	}
	if len(opts.Technologies) > 0 {
		q, qArgs, err := sqlx.In("EXISTS (SELECT 1 FROM technology_usage_view tuv WHERE tuv.program_id = p.id AND tuv.technology IN (?))", opts.Technologies)
		if err != nil {
			return metadata.ProgramsPage{}, fmt.Errorf("technology filter: %w", err)
		}
		filters = append(filters, q)
		args = append(args, qArgs...)
	}

	where := ""
	if len(filters) > 0 {
		where = "WHERE " + strings.Join(filters, " AND ")
	}

	query := `
WITH filtered AS (
        SELECT
                p.id,
                p.project_id,
                p.name,
                p.source_path,
                p.summary,
                p.created_at,
                p.updated_at,
                stats.last_document_update,
                stats.document_count,
                stats.doc_type_count,
                GROUP_CONCAT(DISTINCT tuv.technology) AS techs
        FROM programs p
        LEFT JOIN program_doc_stats stats ON stats.program_id = p.id
        LEFT JOIN technology_usage_view tuv ON tuv.program_id = p.id
        %s
        GROUP BY p.id
)
SELECT *, COUNT(*) OVER() AS total_rows FROM filtered
ORDER BY name
LIMIT ? OFFSET ?`
	query = fmt.Sprintf(query, where)

	args = append(args, limit, offset)

	records := []struct {
		ID                 int64          `db:"id"`
		ProjectID          string         `db:"project_id"`
		Name               string         `db:"name"`
		SourcePath         sql.NullString `db:"source_path"`
		Summary            sql.NullString `db:"summary"`
		CreatedAt          time.Time      `db:"created_at"`
		UpdatedAt          time.Time      `db:"updated_at"`
		LastDocumentUpdate sql.NullTime   `db:"last_document_update"`
		DocumentCount      sql.NullInt64  `db:"document_count"`
		DocTypeCount       sql.NullInt64  `db:"doc_type_count"`
		Technologies       sql.NullString `db:"techs"`
		TotalRows          int            `db:"total_rows"`
	}{}
	if err := s.db.SelectContext(ctx, &records, query, args...); err != nil {
		return metadata.ProgramsPage{}, fmt.Errorf("query programs: %w", err)
	}

	page := metadata.ProgramsPage{Limit: limit, Offset: offset}
	for _, rec := range records {
		record := metadata.ProgramRecord{
			ID:        rec.ID,
			ProjectID: rec.ProjectID,
			Name:      rec.Name,
			CreatedAt: rec.CreatedAt,
			UpdatedAt: rec.UpdatedAt,
		}
		if rec.SourcePath.Valid {
			record.SourcePath = rec.SourcePath.String
		}
		if rec.Summary.Valid {
			record.Summary = rec.Summary.String
		}
		if rec.DocumentCount.Valid {
			record.DocumentCount = int(rec.DocumentCount.Int64)
		}
		if rec.DocTypeCount.Valid {
			record.DocTypeCount = int(rec.DocTypeCount.Int64)
		}
		if rec.LastDocumentUpdate.Valid {
			ts := rec.LastDocumentUpdate.Time
			record.LastDocumentUpdate = &ts
		}
		if rec.Technologies.Valid {
			parts := strings.Split(rec.Technologies.String, ",")
			seen := make(map[string]struct{}, len(parts))
			for _, p := range parts {
				p = strings.TrimSpace(p)
				if p == "" {
					continue
				}
				if _, ok := seen[p]; ok {
					continue
				}
				seen[p] = struct{}{}
				record.Technologies = append(record.Technologies, p)
			}
			if len(record.Technologies) > 1 {
				sort.Strings(record.Technologies)
			}
		}
		page.Programs = append(page.Programs, record)
		page.Total = rec.TotalRows
	}
	return page, nil
}

// StreamPrograms streams program results through the provided callback.
func (s *Store) StreamPrograms(ctx context.Context, opts metadata.QueryOptions, fn func(metadata.ProgramRecord) error) error {
	pageSize := opts.Limit
	if pageSize <= 0 {
		pageSize = 256
	}
	offset := opts.Offset
	for {
		pageOpts := opts
		pageOpts.Limit = pageSize
		pageOpts.Offset = offset
		page, err := s.QueryPrograms(ctx, pageOpts)
		if err != nil {
			return err
		}
		if len(page.Programs) == 0 {
			return nil
		}
		for _, rec := range page.Programs {
			if err := fn(rec); err != nil {
				return err
			}
		}
		offset += len(page.Programs)
		if offset >= page.Total {
			return nil
		}
	}
}

// TechnologyUsage aggregates program counts per technology.
func (s *Store) TechnologyUsage(ctx context.Context, projectID string) ([]metadata.TechnologyUsage, error) {
	if err := s.ensureReady(); err != nil {
		return nil, err
	}
	if strings.TrimSpace(projectID) == "" {
		return nil, fmt.Errorf("project id required")
	}
	query := `
SELECT technology,
       COUNT(DISTINCT program_id) AS program_count,
       SUM(dependency_count) AS relationship_count
FROM technology_dependency_view
WHERE project_id = ?
GROUP BY technology
ORDER BY program_count DESC, technology`
	rows := []struct {
		Technology        string `db:"technology"`
		ProgramCount      int    `db:"program_count"`
		RelationshipCount int    `db:"relationship_count"`
	}{}
	if err := s.db.SelectContext(ctx, &rows, query, projectID); err != nil {
		return nil, fmt.Errorf("technology usage: %w", err)
	}
	result := make([]metadata.TechnologyUsage, 0, len(rows))
	for _, row := range rows {
		result = append(result, metadata.TechnologyUsage{
			Technology:        row.Technology,
			ProgramCount:      row.ProgramCount,
			RelationshipCount: row.RelationshipCount,
		})
	}
	return result, nil
}

// DependencyCounts summarises relationships either for an entire project or a
// single program when provided.
func (s *Store) DependencyCounts(ctx context.Context, projectID string, programID *int64) ([]metadata.DependencyCount, error) {
	if err := s.ensureReady(); err != nil {
		return nil, err
	}
	if strings.TrimSpace(projectID) == "" {
		return nil, fmt.Errorf("project id required")
	}
	base := `
SELECT program_id, kind, MAX(count) AS total
FROM relationship_counts_view
WHERE project_id = ?`
	args := []interface{}{projectID}
	if programID != nil {
		base += " AND program_id = ?"
		args = append(args, *programID)
	}
	base += " GROUP BY program_id, kind ORDER BY program_id, kind"
	rows := []struct {
		ProgramID int64  `db:"program_id"`
		Kind      string `db:"kind"`
		Total     int    `db:"total"`
	}{}
	if err := s.db.SelectContext(ctx, &rows, base, args...); err != nil {
		return nil, fmt.Errorf("dependency counts: %w", err)
	}
	out := make([]metadata.DependencyCount, 0, len(rows))
	for _, row := range rows {
		out = append(out, metadata.DependencyCount{ProgramID: row.ProgramID, Kind: row.Kind, Count: row.Total})
	}
	return out, nil
}

// ChangeHistory returns the most recent change events for a program.
func (s *Store) ChangeHistory(ctx context.Context, programID int64, limit int) ([]metadata.ChangeEvent, error) {
	if err := s.ensureReady(); err != nil {
		return nil, err
	}
	if programID <= 0 {
		return nil, fmt.Errorf("program id required")
	}
	if limit <= 0 {
		limit = 50
	}
	query := `
SELECT program_id, doc_id, doc_type, fingerprint, previous_fingerprint, created_at
FROM file_change_history
WHERE program_id = ?
ORDER BY created_at DESC
LIMIT ?`
	rows := []struct {
		ProgramID   int64     `db:"program_id"`
		DocID       string    `db:"doc_id"`
		DocType     string    `db:"doc_type"`
		Fingerprint string    `db:"fingerprint"`
		Previous    string    `db:"previous_fingerprint"`
		CreatedAt   time.Time `db:"created_at"`
	}{}
	if err := s.db.SelectContext(ctx, &rows, query, programID, limit); err != nil {
		return nil, fmt.Errorf("change history: %w", err)
	}
	out := make([]metadata.ChangeEvent, 0, len(rows))
	for _, row := range rows {
		out = append(out, metadata.ChangeEvent{
			ProgramID:   row.ProgramID,
			DocID:       row.DocID,
			DocType:     row.DocType,
			Fingerprint: row.Fingerprint,
			Previous:    row.Previous,
			CreatedAt:   row.CreatedAt,
		})
	}
	return out, nil
}

// CrossReferences returns relationship matches for a target symbol.
func (s *Store) CrossReferences(ctx context.Context, opts metadata.CrossReferenceOptions) ([]metadata.CrossReference, error) {
	if err := s.ensureReady(); err != nil {
		return nil, err
	}
	if strings.TrimSpace(opts.ProjectID) == "" {
		return nil, fmt.Errorf("project id required")
	}
	if strings.TrimSpace(opts.Target) == "" {
		return nil, fmt.Errorf("target required")
	}
	limit := opts.Limit
	if limit <= 0 {
		limit = 100
	}
	offset := opts.Offset
	if offset < 0 {
		offset = 0
	}
	filters := []string{"rcv.project_id = ?", "rcv.target = ?"}
	args := []interface{}{opts.ProjectID, opts.Target}
	if len(opts.Kinds) > 0 {
		in, inArgs, err := sqlx.In("rcv.kind IN (?)", opts.Kinds)
		if err != nil {
			return nil, fmt.Errorf("cross reference kinds: %w", err)
		}
		filters = append(filters, in)
		args = append(args, inArgs...)
	}
	query := fmt.Sprintf(`
SELECT program_id, program, kind, target, sequence, created_at
FROM relationship_counts_view rcv
WHERE %s
ORDER BY program, sequence
LIMIT ? OFFSET ?`, strings.Join(filters, " AND "))
	args = append(args, limit, offset)
	rows := []struct {
		ProgramID int64     `db:"program_id"`
		Program   string    `db:"program"`
		Kind      string    `db:"kind"`
		Target    string    `db:"target"`
		Sequence  int       `db:"sequence"`
		CreatedAt time.Time `db:"created_at"`
	}{}
	if err := s.db.SelectContext(ctx, &rows, query, args...); err != nil {
		return nil, fmt.Errorf("cross references: %w", err)
	}
	out := make([]metadata.CrossReference, 0, len(rows))
	for _, row := range rows {
		out = append(out, metadata.CrossReference{
			ProgramID: row.ProgramID,
			Program:   row.Program,
			Kind:      row.Kind,
			Target:    row.Target,
			Sequence:  row.Sequence,
			CreatedAt: row.CreatedAt,
		})
	}
	return out, nil
}

// BatchUpsertPrograms inserts or updates multiple program rows and their
// associated technologies within a single transaction.
func (s *Store) BatchUpsertPrograms(ctx context.Context, programs []metadata.ProgramUpsert) error {
	if err := s.ensureReady(); err != nil {
		return err
	}
	if len(programs) == 0 {
		return nil
	}
	tx, err := s.db.BeginTxx(ctx, &sql.TxOptions{})
	if err != nil {
		return fmt.Errorf("begin program upsert: %w", err)
	}
	committed := false
	defer func() {
		if !committed {
			tx.Rollback()
		}
	}()

	for _, program := range programs {
		if strings.TrimSpace(program.ProjectID) == "" || strings.TrimSpace(program.Name) == "" {
			return fmt.Errorf("program project and name required")
		}
		_, execErr := tx.ExecContext(ctx, `
INSERT INTO programs(project_id, name, source_path, summary, updated_at)
VALUES (?, ?, NULLIF(?, ''), NULLIF(?, ''), CURRENT_TIMESTAMP)
ON CONFLICT(project_id, name) DO UPDATE SET
        source_path=excluded.source_path,
        summary=excluded.summary,
        updated_at=CURRENT_TIMESTAMP`,
			program.ProjectID,
			program.Name,
			program.SourcePath,
			program.Summary,
		)
		if execErr != nil {
			return fmt.Errorf("upsert program %s: %w", program.Name, execErr)
		}
		var programID int64
		if err := tx.GetContext(ctx, &programID, `SELECT id FROM programs WHERE project_id = ? AND name = ?`, program.ProjectID, program.Name); err != nil {
			return fmt.Errorf("lookup program id: %w", err)
		}
		for _, tech := range program.Technologies {
			tech = strings.TrimSpace(tech)
			if tech == "" {
				continue
			}
			if _, execErr := tx.ExecContext(ctx, `INSERT INTO technologies(name) VALUES (?) ON CONFLICT(name) DO NOTHING`, tech); execErr != nil {
				return fmt.Errorf("ensure technology %s: %w", tech, execErr)
			}
			if _, execErr := tx.ExecContext(ctx, `
INSERT INTO program_technologies(program_id, technology_id)
SELECT ?, id FROM technologies WHERE name = ?
ON CONFLICT(program_id, technology_id) DO NOTHING`, programID, tech); execErr != nil {
				return fmt.Errorf("link technology %s: %w", tech, execErr)
			}
		}
	}
	if err := tx.Commit(); err != nil {
		return fmt.Errorf("commit program upserts: %w", err)
	}
	committed = true
	return nil
}

// BatchUpsertFiles inserts or updates document rows in bulk and maintains the
// change history table.
func (s *Store) BatchUpsertFiles(ctx context.Context, files []metadata.FileUpsert) error {
	if err := s.ensureReady(); err != nil {
		return err
	}
	if len(files) == 0 {
		return nil
	}
	tx, err := s.db.BeginTxx(ctx, &sql.TxOptions{})
	if err != nil {
		return fmt.Errorf("begin file upsert: %w", err)
	}
	committed := false
	defer func() {
		if !committed {
			tx.Rollback()
		}
	}()

	for _, file := range files {
		if strings.TrimSpace(file.DocID) == "" {
			return fmt.Errorf("doc id required")
		}
		if file.ProgramID <= 0 {
			return fmt.Errorf("program id required for doc %s", file.DocID)
		}
		var existing struct {
			ID          int64  `db:"id"`
			Fingerprint string `db:"fingerprint"`
		}
		getErr := tx.GetContext(ctx, &existing, `SELECT id, fingerprint FROM files WHERE doc_id = ?`, file.DocID)
		if getErr != nil && !errors.Is(getErr, sql.ErrNoRows) {
			return fmt.Errorf("lookup file %s: %w", file.DocID, getErr)
		}
		_, execErr := tx.ExecContext(ctx, `
INSERT INTO files(program_id, doc_id, chunk, doc_type, content, summary, fingerprint, updated_at)
VALUES (?, ?, ?, NULLIF(?, ''), ?, NULLIF(?, ''), NULLIF(?, ''), CURRENT_TIMESTAMP)
ON CONFLICT(doc_id) DO UPDATE SET
        program_id=excluded.program_id,
        chunk=excluded.chunk,
        doc_type=excluded.doc_type,
        content=excluded.content,
        summary=excluded.summary,
        fingerprint=excluded.fingerprint,
        updated_at=CURRENT_TIMESTAMP`,
			file.ProgramID,
			file.DocID,
			file.Chunk,
			file.DocType,
			file.Content,
			file.Summary,
			file.Fingerprint,
		)
		if execErr != nil {
			return fmt.Errorf("upsert file %s: %w", file.DocID, execErr)
		}
		var fileID int64
		if err := tx.GetContext(ctx, &fileID, `SELECT id FROM files WHERE doc_id = ?`, file.DocID); err != nil {
			return fmt.Errorf("lookup file id: %w", err)
		}
		if existing.ID == 0 || existing.Fingerprint != file.Fingerprint {
			_, execErr = tx.ExecContext(ctx, `
INSERT INTO versions(file_id, fingerprint, previous_fingerprint, created_at)
VALUES (?, ?, NULLIF(?, ''), CURRENT_TIMESTAMP)`, fileID, file.Fingerprint, existing.Fingerprint)
			if execErr != nil {
				return fmt.Errorf("insert version for %s: %w", file.DocID, execErr)
			}
		}
	}
	if err := tx.Commit(); err != nil {
		return fmt.Errorf("commit file upserts: %w", err)
	}
	committed = true
	return nil
}
