// File path: third_party/sqlx/sqlx_test.go
package sqlx

import (
	"database/sql"
	"fmt"
	"testing"
	"time"
)

func TestSelectProgramsWithStatsBasicAndPagination(t *testing.T) {
	store := newDataStore()
	seedPrograms(store)

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
        WHERE p.project_id = ?
        GROUP BY p.id
)
SELECT *, COUNT(*) OVER() AS total_rows FROM filtered
ORDER BY name
LIMIT ? OFFSET ?`

	type record struct {
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
	}

	var rows []record
	if err := store.selectQuery(query, &rows, "proj", 10, 0); err != nil {
		t.Fatalf("select query: %v", err)
	}

	if len(rows) != 3 {
		t.Fatalf("expected 3 rows, got %d", len(rows))
	}

	if rows[0].Name != "Alpha" || rows[1].Name != "Beta" || rows[2].Name != "Gamma" {
		t.Fatalf("unexpected ordering: %+v", rows)
	}

	if got := rows[0].DocumentCount; !got.Valid || got.Int64 != 2 {
		t.Errorf("alpha document count = %+v, want 2", got)
	}
	if got := rows[0].DocTypeCount; !got.Valid || got.Int64 != 2 {
		t.Errorf("alpha doc type count = %+v, want 2", got)
	}
	if got := rows[0].LastDocumentUpdate; !got.Valid || !got.Time.Equal(seedTime.Add(48*time.Hour)) {
		t.Errorf("alpha last document update = %+v", got)
	}
	if got := rows[0].Technologies; !got.Valid || got.String != "Go,React" {
		t.Errorf("alpha technologies = %+v", got)
	}

	if got := rows[2].LastDocumentUpdate; got.Valid {
		t.Errorf("gamma last document update should be NULL, got %+v", got)
	}
	if got := rows[2].DocumentCount; !got.Valid || got.Int64 != 0 {
		t.Errorf("gamma document count = %+v, want 0", got)
	}
	if got := rows[2].Technologies; got.Valid {
		t.Errorf("gamma technologies should be NULL, got %+v", got)
	}

	for i, row := range rows {
		if row.TotalRows != 3 {
			t.Errorf("row %d total rows = %d, want 3", i, row.TotalRows)
		}
	}

	rows = nil
	if err := store.selectQuery(query, &rows, "proj", 1, 1); err != nil {
		t.Fatalf("select query with pagination: %v", err)
	}
	if len(rows) != 1 {
		t.Fatalf("expected 1 row after pagination, got %d", len(rows))
	}
	if rows[0].Name != "Beta" {
		t.Fatalf("expected Beta after pagination, got %s", rows[0].Name)
	}
	if rows[0].TotalRows != 3 {
		t.Fatalf("expected total rows 3 after pagination, got %d", rows[0].TotalRows)
	}
}

func TestSelectProgramsWithStatsFilters(t *testing.T) {
	store := newDataStore()
	seedPrograms(store)

	base := `
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

	type record struct {
		Name      string        `db:"name"`
		TotalRows int           `db:"total_rows"`
		DocCount  sql.NullInt64 `db:"document_count"`
	}

	tests := []struct {
		name   string
		where  string
		args   []interface{}
		expect []string
	}{
		{
			name:   "name pattern",
			where:  "WHERE p.project_id = ? AND p.name LIKE ?",
			args:   []interface{}{"proj", "B%"},
			expect: []string{"Beta"},
		},
		{
			name:   "updated after",
			where:  "WHERE p.project_id = ? AND stats.last_document_update >= ?",
			args:   []interface{}{"proj", seedTime.Add(60 * time.Hour)},
			expect: []string{"Beta"},
		},
		{
			name:   "updated before",
			where:  "WHERE p.project_id = ? AND stats.last_document_update <= ?",
			args:   []interface{}{"proj", seedTime.Add(50 * time.Hour)},
			expect: []string{"Alpha"},
		},
		{
			name:   "doc type filter",
			where:  "WHERE p.project_id = ? AND EXISTS (SELECT 1 FROM files f2 WHERE f2.program_id = p.id AND f2.doc_type IN (?))",
			args:   []interface{}{"proj", "guide"},
			expect: []string{"Alpha"},
		},
		{
			name:   "technology filter",
			where:  "WHERE p.project_id = ? AND EXISTS (SELECT 1 FROM technology_usage_view tuv WHERE tuv.program_id = p.id AND tuv.technology IN (?))",
			args:   []interface{}{"proj", "React"},
			expect: []string{"Alpha"},
		},
		{
			name:   "combined filters",
			where:  "WHERE p.project_id = ? AND EXISTS (SELECT 1 FROM files f2 WHERE f2.program_id = p.id AND f2.doc_type IN (?)) AND EXISTS (SELECT 1 FROM technology_usage_view tuv WHERE tuv.program_id = p.id AND tuv.technology IN (?))",
			args:   []interface{}{"proj", "guide", "React"},
			expect: []string{"Alpha"},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			query := fmt.Sprintf(base, tc.where)
			var rows []record
			args := append([]interface{}{}, tc.args...)
			args = append(args, 100, 0)
			if err := store.selectQuery(query, &rows, args...); err != nil {
				t.Fatalf("select query: %v", err)
			}
			if len(rows) != len(tc.expect) {
				t.Fatalf("expected %d rows, got %d", len(tc.expect), len(rows))
			}
			for i, row := range rows {
				if row.Name != tc.expect[i] {
					t.Fatalf("row %d name = %s, want %s", i, row.Name, tc.expect[i])
				}
				if row.TotalRows != len(tc.expect) {
					t.Fatalf("row %d total rows = %d, want %d", i, row.TotalRows, len(tc.expect))
				}
			}
		})
	}
}

var seedTime = time.Date(2024, time.January, 1, 0, 0, 0, 0, time.UTC)

func seedPrograms(store *dataStore) {
	store.programs[1] = &programRow{ID: 1, ProjectID: "proj", Name: "Alpha", SourcePath: "alpha", CreatedAt: seedTime, UpdatedAt: seedTime}
	store.programs[2] = &programRow{ID: 2, ProjectID: "proj", Name: "Beta", SourcePath: "beta", CreatedAt: seedTime, UpdatedAt: seedTime}
	store.programs[3] = &programRow{ID: 3, ProjectID: "proj", Name: "Gamma", CreatedAt: seedTime, UpdatedAt: seedTime}
	store.programs[4] = &programRow{ID: 4, ProjectID: "other", Name: "Other", CreatedAt: seedTime, UpdatedAt: seedTime}

	store.files[1] = &fileRow{ID: 1, ProgramID: 1, DocType: "code", UpdatedAt: seedTime.Add(24 * time.Hour)}
	store.files[2] = &fileRow{ID: 2, ProgramID: 1, DocType: "guide", UpdatedAt: seedTime.Add(48 * time.Hour)}
	store.files[3] = &fileRow{ID: 3, ProgramID: 2, DocType: "code", UpdatedAt: seedTime.Add(72 * time.Hour)}

	store.programTechnologies[1] = map[int64]struct{}{1: {}, 2: {}}
	store.programTechnologies[2] = map[int64]struct{}{1: {}}

	store.technologies[1] = &technologyRow{ID: 1, Name: "Go"}
	store.technologies[2] = &technologyRow{ID: 2, Name: "React"}
}
