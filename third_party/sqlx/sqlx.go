// File path: third_party/sqlx/sqlx.go
package sqlx

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"reflect"
	"sort"
	"strings"
	"sync"
	"time"
)

type DB struct {
	mu    sync.RWMutex
	store *dataStore
}

type Tx struct {
	db     *DB
	store  *dataStore
	closed bool
}

type result struct {
	lastID int64
	rows   int64
}

func (r result) LastInsertId() (int64, error) {
	return r.lastID, nil
}

func (r result) RowsAffected() (int64, error) {
	return r.rows, nil
}

func Open(driverName, dataSourceName string) (*DB, error) {
	return &DB{store: newDataStore()}, nil
}

func (db *DB) SetMaxOpenConns(n int)              {}
func (db *DB) SetMaxIdleConns(n int)              {}
func (db *DB) SetConnMaxLifetime(d time.Duration) {}
func (db *DB) SetConnMaxIdleTime(d time.Duration) {}

func (db *DB) PingContext(ctx context.Context) error {
	return nil
}

func (db *DB) Close() error {
	return nil
}

func (db *DB) BeginTxx(ctx context.Context, opts *sql.TxOptions) (*Tx, error) {
	db.mu.RLock()
	defer db.mu.RUnlock()
	clone := db.store.clone()
	return &Tx{db: db, store: clone}, nil
}

func (db *DB) ExecContext(ctx context.Context, query string, args ...interface{}) (sql.Result, error) {
	db.mu.Lock()
	defer db.mu.Unlock()
	res, err := db.store.exec(query, args...)
	if err != nil {
		return nil, err
	}
	return res, nil
}

func (db *DB) SelectContext(ctx context.Context, dest interface{}, query string, args ...interface{}) error {
	db.mu.RLock()
	defer db.mu.RUnlock()
	return db.store.selectQuery(query, dest, args...)
}

func (db *DB) GetContext(ctx context.Context, dest interface{}, query string, args ...interface{}) error {
	db.mu.RLock()
	defer db.mu.RUnlock()
	return db.store.getQuery(query, dest, args...)
}

func (db *DB) Rebind(query string) string {
	return query
}

func (tx *Tx) ExecContext(ctx context.Context, query string, args ...interface{}) (sql.Result, error) {
	if tx.closed {
		return nil, errors.New("sqlx: transaction closed")
	}
	return tx.store.exec(query, args...)
}

func (tx *Tx) SelectContext(ctx context.Context, dest interface{}, query string, args ...interface{}) error {
	if tx.closed {
		return errors.New("sqlx: transaction closed")
	}
	return tx.store.selectQuery(query, dest, args...)
}

func (tx *Tx) GetContext(ctx context.Context, dest interface{}, query string, args ...interface{}) error {
	if tx.closed {
		return errors.New("sqlx: transaction closed")
	}
	return tx.store.getQuery(query, dest, args...)
}

func (tx *Tx) Commit() error {
	if tx.closed {
		return errors.New("sqlx: transaction already closed")
	}
	tx.db.mu.Lock()
	tx.db.store = tx.store
	tx.db.mu.Unlock()
	tx.closed = true
	return nil
}

func (tx *Tx) Rollback() error {
	if tx.closed {
		return errors.New("sqlx: transaction already closed")
	}
	tx.closed = true
	return nil
}

type dataStore struct {
	nextProgramID      int64
	nextFileID         int64
	nextTechnologyID   int64
	nextRelationshipID int64
	nextVersionID      int64
	nextAuditID        int64

	programs     map[int64]*programRow
	programIndex map[string]int64

	files     map[int64]*fileRow
	fileIndex map[string]int64

	technologies    map[int64]*technologyRow
	technologyIndex map[string]int64

	programTechnologies map[int64]map[int64]struct{}

	relationships          map[int64]*relationshipRow
	relationshipsByProgram map[int64][]int64

	versions map[int64]*versionRow
	audit    map[int64]*auditRow
}

type programRow struct {
	ID         int64
	ProjectID  string
	Name       string
	SourcePath string
	Summary    string
	CreatedAt  time.Time
	UpdatedAt  time.Time
}

type fileRow struct {
	ID          int64
	ProgramID   int64
	DocID       string
	Chunk       int
	DocType     string
	Content     string
	Summary     string
	Fingerprint string
	CreatedAt   time.Time
	UpdatedAt   time.Time
}

type technologyRow struct {
	ID   int64
	Name string
}

type relationshipRow struct {
	ID        int64
	ProgramID int64
	Kind      string
	Target    string
	Sequence  int
	CreatedAt time.Time
}

type versionRow struct {
	ID                  int64
	FileID              int64
	Fingerprint         string
	PreviousFingerprint string
	CreatedAt           time.Time
}

type auditRow struct {
	ID        int64
	ProjectID string
	ProgramID *int64
	Action    string
	Detail    string
	CreatedAt time.Time
}

func newDataStore() *dataStore {
	return &dataStore{
		programs:               make(map[int64]*programRow),
		programIndex:           make(map[string]int64),
		files:                  make(map[int64]*fileRow),
		fileIndex:              make(map[string]int64),
		technologies:           make(map[int64]*technologyRow),
		technologyIndex:        make(map[string]int64),
		programTechnologies:    make(map[int64]map[int64]struct{}),
		relationships:          make(map[int64]*relationshipRow),
		relationshipsByProgram: make(map[int64][]int64),
		versions:               make(map[int64]*versionRow),
		audit:                  make(map[int64]*auditRow),
	}
}

func (s *dataStore) clone() *dataStore {
	cloned := newDataStore()
	cloned.nextProgramID = s.nextProgramID
	cloned.nextFileID = s.nextFileID
	cloned.nextTechnologyID = s.nextTechnologyID
	cloned.nextRelationshipID = s.nextRelationshipID
	cloned.nextVersionID = s.nextVersionID
	cloned.nextAuditID = s.nextAuditID

	for id, row := range s.programs {
		copied := *row
		cloned.programs[id] = &copied
	}
	for key, id := range s.programIndex {
		cloned.programIndex[key] = id
	}
	for id, row := range s.files {
		copied := *row
		cloned.files[id] = &copied
	}
	for key, id := range s.fileIndex {
		cloned.fileIndex[key] = id
	}
	for id, row := range s.technologies {
		copied := *row
		cloned.technologies[id] = &copied
	}
	for key, id := range s.technologyIndex {
		cloned.technologyIndex[key] = id
	}
	for programID, set := range s.programTechnologies {
		newSet := make(map[int64]struct{}, len(set))
		for techID := range set {
			newSet[techID] = struct{}{}
		}
		cloned.programTechnologies[programID] = newSet
	}
	for id, row := range s.relationships {
		copied := *row
		cloned.relationships[id] = &copied
	}
	for programID, ids := range s.relationshipsByProgram {
		newSlice := make([]int64, len(ids))
		copy(newSlice, ids)
		cloned.relationshipsByProgram[programID] = newSlice
	}
	for id, row := range s.versions {
		copied := *row
		cloned.versions[id] = &copied
	}
	for id, row := range s.audit {
		var programID *int64
		if row.ProgramID != nil {
			val := *row.ProgramID
			programID = &val
		}
		cloned.audit[id] = &auditRow{
			ID:        row.ID,
			ProjectID: row.ProjectID,
			ProgramID: programID,
			Action:    row.Action,
			Detail:    row.Detail,
			CreatedAt: row.CreatedAt,
		}
	}
	return cloned
}

func (s *dataStore) exec(query string, args ...interface{}) (sql.Result, error) {
	trimmed := strings.TrimSpace(query)
	switch {
	case strings.HasPrefix(strings.ToUpper(trimmed), "PRAGMA"):
		return result{}, nil
	case strings.HasPrefix(strings.ToUpper(trimmed), "CREATE TABLE"):
		return result{}, nil
	case strings.HasPrefix(strings.ToUpper(trimmed), "CREATE INDEX"):
		return result{}, nil
	case strings.HasPrefix(strings.ToUpper(trimmed), "CREATE VIEW"):
		return result{}, nil
	case strings.HasPrefix(trimmed, "INSERT INTO programs"):
		return s.execInsertProgram(args...)
	case trimmed == "DELETE FROM program_technologies WHERE program_id = ?":
		return s.execDeleteProgramTechnologies(args...)
	case strings.HasPrefix(trimmed, "INSERT INTO technologies"):
		return s.execInsertTechnology(args...)
	case strings.HasPrefix(trimmed, "INSERT OR IGNORE INTO program_technologies"):
		return s.execInsertProgramTechnology(args...)
	case strings.HasPrefix(trimmed, "DELETE FROM relationships WHERE program_id = ? AND kind = ?"):
		return s.execDeleteRelationships(args...)
	case strings.HasPrefix(trimmed, "INSERT INTO relationships"):
		return s.execInsertRelationship(args...)
	case strings.HasPrefix(trimmed, "INSERT INTO files"):
		return s.execInsertFile(args...)
	case strings.HasPrefix(trimmed, "UPDATE files SET"):
		return s.execUpdateFile(args...)
	case strings.HasPrefix(trimmed, "INSERT INTO versions"):
		return s.execInsertVersion(args...)
	case strings.HasPrefix(trimmed, "INSERT INTO audit(project_id, action, detail)") && strings.Contains(trimmed, "schema_created"):
		return s.execInsertInitialAudit()
	case strings.HasPrefix(trimmed, "INSERT INTO audit"):
		return s.execInsertAudit(args...)
	default:
		return nil, fmt.Errorf("sqlx: unsupported exec query: %s", trimmed)
	}
}

func (s *dataStore) selectQuery(query string, dest interface{}, args ...interface{}) error {
	trimmed := strings.TrimSpace(query)
	switch {
	case trimmed == "SELECT * FROM programs WHERE project_id = ? ORDER BY name":
		return s.selectPrograms(dest, args...)
	case trimmed == "SELECT name FROM programs WHERE project_id = ?":
		return s.selectProgramNames(dest, args...)
	case trimmed == "SELECT * FROM files WHERE program_id = ? ORDER BY chunk, id":
		return s.selectFiles(dest, args...)
	case strings.HasPrefix(trimmed, "SELECT * FROM relationships WHERE program_id = ? AND kind IN"):
		return s.selectRelationshipsWithKinds(dest, args...)
	case trimmed == "SELECT * FROM relationships WHERE program_id = ? ORDER BY kind, sequence":
		return s.selectRelationships(dest, args...)
	case strings.HasPrefix(trimmed, "SELECT t.name FROM technologies t"):
		return s.selectTechnologies(dest, args...)
	case strings.HasPrefix(trimmed, "WITH filtered AS (") && strings.Contains(trimmed, "SELECT *, COUNT(*) OVER() AS total_rows FROM filtered"):
		return s.selectProgramsWithStats(dest, trimmed, args...)
	default:
		return fmt.Errorf("sqlx: unsupported select query: %s", trimmed)
	}
}

func (s *dataStore) getQuery(query string, dest interface{}, args ...interface{}) error {
	trimmed := strings.TrimSpace(query)
	switch {
	case trimmed == "SELECT id FROM programs WHERE project_id = ? AND name = ?":
		return s.getProgramID(dest, args...)
	case trimmed == "SELECT id FROM technologies WHERE name = ?":
		return s.getTechnologyID(dest, args...)
	case trimmed == "SELECT id, fingerprint FROM files WHERE doc_id = ?":
		return s.getFileFingerprint(dest, args...)
	case trimmed == "SELECT * FROM programs WHERE project_id = ? AND name = ?":
		return s.getProgram(dest, args...)
	default:
		return fmt.Errorf("sqlx: unsupported get query: %s", trimmed)
	}
}

func (s *dataStore) execInsertProgram(args ...interface{}) (sql.Result, error) {
	if len(args) < 4 {
		return nil, fmt.Errorf("sqlx: insert program args")
	}
	projectID := asString(args[0])
	name := asString(args[1])
	sourcePath := asString(args[2])
	summary := asString(args[3])
	key := programKey(projectID, name)
	now := time.Now().UTC()
	if id, ok := s.programIndex[key]; ok {
		row := s.programs[id]
		row.SourcePath = sourcePath
		if summary != "" {
			row.Summary = summary
		}
		row.UpdatedAt = now
		return result{rows: 1}, nil
	}
	s.nextProgramID++
	id := s.nextProgramID
	row := &programRow{
		ID:         id,
		ProjectID:  projectID,
		Name:       name,
		SourcePath: sourcePath,
		Summary:    summary,
		CreatedAt:  now,
		UpdatedAt:  now,
	}
	s.programs[id] = row
	s.programIndex[key] = id
	return result{lastID: id, rows: 1}, nil
}

func (s *dataStore) execDeleteProgramTechnologies(args ...interface{}) (sql.Result, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("sqlx: delete program technologies args")
	}
	programID := asInt64(args[0])
	count := int64(len(s.programTechnologies[programID]))
	delete(s.programTechnologies, programID)
	return result{rows: count}, nil
}

func (s *dataStore) execInsertTechnology(args ...interface{}) (sql.Result, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("sqlx: insert technology args")
	}
	name := asString(args[0])
	if name == "" {
		return result{}, nil
	}
	if id, ok := s.technologyIndex[strings.ToLower(name)]; ok {
		return result{lastID: id, rows: 0}, nil
	}
	s.nextTechnologyID++
	id := s.nextTechnologyID
	row := &technologyRow{ID: id, Name: name}
	s.technologies[id] = row
	s.technologyIndex[strings.ToLower(name)] = id
	return result{lastID: id, rows: 1}, nil
}

func (s *dataStore) execInsertProgramTechnology(args ...interface{}) (sql.Result, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("sqlx: insert program technology args")
	}
	programID := asInt64(args[0])
	technologyID := asInt64(args[1])
	set := s.programTechnologies[programID]
	if set == nil {
		set = make(map[int64]struct{})
		s.programTechnologies[programID] = set
	}
	if _, exists := set[technologyID]; exists {
		return result{rows: 0}, nil
	}
	set[technologyID] = struct{}{}
	return result{rows: 1}, nil
}

func (s *dataStore) execDeleteRelationships(args ...interface{}) (sql.Result, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("sqlx: delete relationships args")
	}
	programID := asInt64(args[0])
	kind := strings.ToLower(asString(args[1]))
	ids := s.relationshipsByProgram[programID]
	var kept []int64
	var removed int64
	for _, id := range ids {
		row := s.relationships[id]
		if strings.ToLower(row.Kind) == kind {
			delete(s.relationships, id)
			removed++
			continue
		}
		kept = append(kept, id)
	}
	if removed > 0 {
		s.relationshipsByProgram[programID] = kept
	}
	return result{rows: removed}, nil
}

func (s *dataStore) execInsertRelationship(args ...interface{}) (sql.Result, error) {
	if len(args) < 4 {
		return nil, fmt.Errorf("sqlx: insert relationship args")
	}
	programID := asInt64(args[0])
	kind := asString(args[1])
	target := asString(args[2])
	sequence := int(asInt64(args[3]))
	s.nextRelationshipID++
	id := s.nextRelationshipID
	row := &relationshipRow{
		ID:        id,
		ProgramID: programID,
		Kind:      kind,
		Target:    target,
		Sequence:  sequence,
		CreatedAt: time.Now().UTC(),
	}
	s.relationships[id] = row
	s.relationshipsByProgram[programID] = append(s.relationshipsByProgram[programID], id)
	return result{lastID: id, rows: 1}, nil
}

func (s *dataStore) execInsertFile(args ...interface{}) (sql.Result, error) {
	if len(args) < 7 {
		return nil, fmt.Errorf("sqlx: insert file args")
	}
	programID := asInt64(args[0])
	docID := asString(args[1])
	chunk := int(asInt64(args[2]))
	docType := asString(args[3])
	content := asString(args[4])
	summary := asString(args[5])
	fingerprint := asString(args[6])
	if _, exists := s.fileIndex[docID]; exists {
		return nil, fmt.Errorf("sqlx: file already exists")
	}
	now := time.Now().UTC()
	s.nextFileID++
	id := s.nextFileID
	row := &fileRow{
		ID:          id,
		ProgramID:   programID,
		DocID:       docID,
		Chunk:       chunk,
		DocType:     docType,
		Content:     content,
		Summary:     summary,
		Fingerprint: fingerprint,
		CreatedAt:   now,
		UpdatedAt:   now,
	}
	s.files[id] = row
	s.fileIndex[docID] = id
	return result{lastID: id, rows: 1}, nil
}

func (s *dataStore) execUpdateFile(args ...interface{}) (sql.Result, error) {
	if len(args) < 7 {
		return nil, fmt.Errorf("sqlx: update file args")
	}
	programID := asInt64(args[0])
	chunk := int(asInt64(args[1]))
	docType := asString(args[2])
	content := asString(args[3])
	summary := asString(args[4])
	fingerprint := asString(args[5])
	id := asInt64(args[6])
	row, ok := s.files[id]
	if !ok {
		return nil, sql.ErrNoRows
	}
	row.ProgramID = programID
	row.Chunk = chunk
	row.DocType = docType
	row.Content = content
	row.Summary = summary
	row.Fingerprint = fingerprint
	row.UpdatedAt = time.Now().UTC()
	return result{rows: 1}, nil
}

func (s *dataStore) execInsertVersion(args ...interface{}) (sql.Result, error) {
	if len(args) < 3 {
		return nil, fmt.Errorf("sqlx: insert version args")
	}
	fileID := asInt64(args[0])
	fingerprint := asString(args[1])
	previous := asString(args[2])
	s.nextVersionID++
	id := s.nextVersionID
	row := &versionRow{
		ID:                  id,
		FileID:              fileID,
		Fingerprint:         fingerprint,
		PreviousFingerprint: previous,
		CreatedAt:           time.Now().UTC(),
	}
	s.versions[id] = row
	return result{lastID: id, rows: 1}, nil
}

func (s *dataStore) execInsertInitialAudit() (sql.Result, error) {
	for _, row := range s.audit {
		if row.Action == "schema_created" {
			return result{rows: 0}, nil
		}
	}
	s.nextAuditID++
	id := s.nextAuditID
	row := &auditRow{
		ID:        id,
		ProjectID: "",
		Action:    "schema_created",
		Detail:    "initial schema loaded",
		CreatedAt: time.Now().UTC(),
	}
	s.audit[id] = row
	return result{lastID: id, rows: 1}, nil
}

func (s *dataStore) execInsertAudit(args ...interface{}) (sql.Result, error) {
	if len(args) < 3 {
		return nil, fmt.Errorf("sqlx: insert audit args")
	}
	projectID := asString(args[0])
	var (
		programID   *int64
		actionIndex int
		detailIndex int
	)
	if len(args) == 3 {
		actionIndex = 1
		detailIndex = 2
	} else {
		if len(args) < 4 {
			return nil, fmt.Errorf("sqlx: insert audit args")
		}
		if args[1] != nil {
			id := asInt64(args[1])
			programID = &id
		}
		actionIndex = 2
		detailIndex = 3
	}
	action := asString(args[actionIndex])
	detail := ""
	if len(args) > detailIndex {
		detail = asString(args[detailIndex])
	}
	s.nextAuditID++
	id := s.nextAuditID
	row := &auditRow{
		ID:        id,
		ProjectID: projectID,
		ProgramID: programID,
		Action:    action,
		Detail:    detail,
		CreatedAt: time.Now().UTC(),
	}
	s.audit[id] = row
	return result{lastID: id, rows: 1}, nil
}

func (s *dataStore) selectPrograms(dest interface{}, args ...interface{}) error {
	projectID := asString(args[0])
	var rows []programRow
	for _, row := range s.programs {
		if row.ProjectID == projectID {
			rows = append(rows, *row)
		}
	}
	sort.Slice(rows, func(i, j int) bool {
		return rows[i].Name < rows[j].Name
	})
	return assignSlice(dest, rows)
}

func (s *dataStore) selectProgramNames(dest interface{}, args ...interface{}) error {
	projectID := asString(args[0])
	type nameRow struct {
		Name string
	}
	var rows []nameRow
	for _, row := range s.programs {
		if row.ProjectID == projectID {
			rows = append(rows, nameRow{Name: row.Name})
		}
	}
	sort.Slice(rows, func(i, j int) bool {
		return rows[i].Name < rows[j].Name
	})
	return assignSlice(dest, rows)
}

func (s *dataStore) selectFiles(dest interface{}, args ...interface{}) error {
	programID := asInt64(args[0])
	var rows []fileRow
	for _, row := range s.files {
		if row.ProgramID == programID {
			rows = append(rows, *row)
		}
	}
	sort.Slice(rows, func(i, j int) bool {
		if rows[i].Chunk == rows[j].Chunk {
			return rows[i].ID < rows[j].ID
		}
		return rows[i].Chunk < rows[j].Chunk
	})
	return assignSlice(dest, rows)
}

func (s *dataStore) selectRelationships(dest interface{}, args ...interface{}) error {
	programID := asInt64(args[0])
	var rows []relationshipRow
	for _, id := range s.relationshipsByProgram[programID] {
		rows = append(rows, *s.relationships[id])
	}
	sort.Slice(rows, func(i, j int) bool {
		if rows[i].Kind == rows[j].Kind {
			return rows[i].Sequence < rows[j].Sequence
		}
		return rows[i].Kind < rows[j].Kind
	})
	return assignSlice(dest, rows)
}

func (s *dataStore) selectRelationshipsWithKinds(dest interface{}, args ...interface{}) error {
	if len(args) < 1 {
		return fmt.Errorf("sqlx: relationships args")
	}
	programID := asInt64(args[0])
	kinds := make(map[string]struct{})
	for _, arg := range args[1:] {
		kinds[strings.ToLower(asString(arg))] = struct{}{}
	}
	var rows []relationshipRow
	for _, id := range s.relationshipsByProgram[programID] {
		row := s.relationships[id]
		if _, ok := kinds[strings.ToLower(row.Kind)]; ok {
			rows = append(rows, *row)
		}
	}
	sort.Slice(rows, func(i, j int) bool {
		if rows[i].Kind == rows[j].Kind {
			return rows[i].Sequence < rows[j].Sequence
		}
		return rows[i].Kind < rows[j].Kind
	})
	return assignSlice(dest, rows)
}

func (s *dataStore) selectTechnologies(dest interface{}, args ...interface{}) error {
	programID := asInt64(args[0])
	set := s.programTechnologies[programID]
	var names []string
	for techID := range set {
		if row, ok := s.technologies[techID]; ok {
			names = append(names, row.Name)
		}
	}
	sort.Strings(names)
	switch d := dest.(type) {
	case *[]string:
		*d = append((*d)[:0], names...)
	default:
		return assignSlice(dest, names)
	}
	return nil
}

func (s *dataStore) selectProgramsWithStats(dest interface{}, query string, args ...interface{}) error {
	if len(args) < 3 {
		return fmt.Errorf("sqlx: select programs stats args")
	}
	lowerQuery := strings.ToLower(query)
	idx := 0
	projectID := asString(args[idx])
	idx++

	hasNameFilter := strings.Contains(lowerQuery, "p.name like ?")
	hasUpdatedAfter := strings.Contains(lowerQuery, "stats.last_document_update >= ?")
	hasUpdatedBefore := strings.Contains(lowerQuery, "stats.last_document_update <= ?")
	docTypePlaceholderCount := countPlaceholders(query, "f2.doc_type in (")
	techPlaceholderCount := countPlaceholders(query, "tuv.technology in (")

	var namePattern string
	if hasNameFilter {
		if idx >= len(args) {
			return fmt.Errorf("sqlx: name filter args")
		}
		namePattern = asString(args[idx])
		idx++
	}

	var updatedAfter *time.Time
	if hasUpdatedAfter {
		if idx >= len(args) {
			return fmt.Errorf("sqlx: updated after args")
		}
		ts, ok := asTime(args[idx])
		if !ok {
			return fmt.Errorf("sqlx: updated after arg type")
		}
		updatedAfter = &ts
		idx++
	}

	var updatedBefore *time.Time
	if hasUpdatedBefore {
		if idx >= len(args) {
			return fmt.Errorf("sqlx: updated before args")
		}
		ts, ok := asTime(args[idx])
		if !ok {
			return fmt.Errorf("sqlx: updated before arg type")
		}
		updatedBefore = &ts
		idx++
	}

	var docTypeFilters []string
	if docTypePlaceholderCount > 0 {
		if idx+docTypePlaceholderCount > len(args) {
			return fmt.Errorf("sqlx: doc type filter args")
		}
		for i := 0; i < docTypePlaceholderCount; i++ {
			docTypeFilters = append(docTypeFilters, asString(args[idx]))
			idx++
		}
	}

	var technologyFilters []string
	if techPlaceholderCount > 0 {
		if idx+techPlaceholderCount > len(args) {
			return fmt.Errorf("sqlx: technology filter args")
		}
		for i := 0; i < techPlaceholderCount; i++ {
			technologyFilters = append(technologyFilters, asString(args[idx]))
			idx++
		}
	}

	if idx+2 > len(args) {
		return fmt.Errorf("sqlx: limit offset args")
	}
	limit := int(asInt64(args[idx]))
	idx++
	offset := int(asInt64(args[idx]))

	type programStatsRow struct {
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

	var rows []programStatsRow
	for _, program := range s.programs {
		if program.ProjectID != projectID {
			continue
		}
		if hasNameFilter && !likeMatch(program.Name, namePattern) {
			continue
		}

		var (
			documentCount int
			docTypes      = make(map[string]struct{})
			lastUpdate    time.Time
			hasDocs       bool
		)
		for _, file := range s.files {
			if file.ProgramID != program.ID {
				continue
			}
			documentCount++
			docTypes[file.DocType] = struct{}{}
			if !hasDocs || file.UpdatedAt.After(lastUpdate) {
				lastUpdate = file.UpdatedAt
				hasDocs = true
			}
		}

		if updatedAfter != nil {
			if !hasDocs || lastUpdate.Before(*updatedAfter) {
				continue
			}
		}
		if updatedBefore != nil {
			if !hasDocs || lastUpdate.After(*updatedBefore) {
				continue
			}
		}

		if len(docTypeFilters) > 0 {
			matched := false
			for _, filter := range docTypeFilters {
				if _, ok := docTypes[filter]; ok {
					matched = true
					break
				}
			}
			if !matched {
				continue
			}
		}

		techSet := s.programTechnologies[program.ID]
		seenTech := make(map[string]struct{}, len(techSet))
		var techNames []string
		for techID := range techSet {
			techRow, ok := s.technologies[techID]
			if !ok {
				continue
			}
			name := techRow.Name
			if _, exists := seenTech[name]; exists {
				continue
			}
			seenTech[name] = struct{}{}
			techNames = append(techNames, name)
		}
		sort.Strings(techNames)

		if len(technologyFilters) > 0 {
			matched := false
			for _, filter := range technologyFilters {
				if _, ok := seenTech[filter]; ok {
					matched = true
					break
				}
			}
			if !matched {
				continue
			}
		}

		var lastUpdateValue sql.NullTime
		if hasDocs {
			lastUpdateValue = sql.NullTime{Time: lastUpdate, Valid: true}
		}
		docCountValue := sql.NullInt64{Int64: int64(documentCount), Valid: true}
		docTypeCountValue := sql.NullInt64{Int64: int64(len(docTypes)), Valid: true}

		var technologies sql.NullString
		if len(techNames) > 0 {
			technologies = sql.NullString{String: strings.Join(techNames, ","), Valid: true}
		}

		row := programStatsRow{
			ID:                 program.ID,
			ProjectID:          program.ProjectID,
			Name:               program.Name,
			SourcePath:         sql.NullString{String: program.SourcePath, Valid: program.SourcePath != ""},
			Summary:            sql.NullString{String: program.Summary, Valid: program.Summary != ""},
			CreatedAt:          program.CreatedAt,
			UpdatedAt:          program.UpdatedAt,
			LastDocumentUpdate: lastUpdateValue,
			DocumentCount:      docCountValue,
			DocTypeCount:       docTypeCountValue,
			Technologies:       technologies,
		}
		rows = append(rows, row)
	}

	sort.Slice(rows, func(i, j int) bool {
		return rows[i].Name < rows[j].Name
	})

	total := len(rows)
	start := offset
	if start < 0 {
		start = 0
	}
	if start > total {
		start = total
	}
	end := total
	if limit > 0 && start+limit < end {
		end = start + limit
	}
	if limit <= 0 {
		start = 0
		end = 0
	}
	paged := rows[start:end]
	for i := range paged {
		paged[i].TotalRows = total
	}
	return assignSlice(dest, paged)
}

func (s *dataStore) getProgramID(dest interface{}, args ...interface{}) error {
	if len(args) < 2 {
		return fmt.Errorf("sqlx: program id args")
	}
	projectID := asString(args[0])
	name := asString(args[1])
	key := programKey(projectID, name)
	id, ok := s.programIndex[key]
	if !ok {
		return sql.ErrNoRows
	}
	switch d := dest.(type) {
	case *int64:
		*d = id
		return nil
	default:
		return assignValue(dest, id)
	}
}

func (s *dataStore) getProgram(dest interface{}, args ...interface{}) error {
	if len(args) < 2 {
		return fmt.Errorf("sqlx: program args")
	}
	projectID := asString(args[0])
	name := asString(args[1])
	key := programKey(projectID, name)
	id, ok := s.programIndex[key]
	if !ok {
		return sql.ErrNoRows
	}
	row := s.programs[id]
	return assignValue(dest, programRowToStruct(row))
}

func (s *dataStore) getTechnologyID(dest interface{}, args ...interface{}) error {
	if len(args) < 1 {
		return fmt.Errorf("sqlx: technology id args")
	}
	name := strings.ToLower(asString(args[0]))
	id, ok := s.technologyIndex[name]
	if !ok {
		return sql.ErrNoRows
	}
	switch d := dest.(type) {
	case *int64:
		*d = id
	default:
		return assignValue(dest, id)
	}
	return nil
}

func (s *dataStore) getFileFingerprint(dest interface{}, args ...interface{}) error {
	if len(args) < 1 {
		return fmt.Errorf("sqlx: file fingerprint args")
	}
	docID := asString(args[0])
	id, ok := s.fileIndex[docID]
	if !ok {
		return sql.ErrNoRows
	}
	row := s.files[id]
	value := struct {
		ID          int64  `db:"id"`
		Fingerprint string `db:"fingerprint"`
	}{ID: row.ID, Fingerprint: row.Fingerprint}
	return assignValue(dest, value)
}

func programKey(projectID, name string) string {
	return strings.ToLower(projectID) + "\x00" + strings.ToLower(name)
}

func asString(v interface{}) string {
	switch val := v.(type) {
	case string:
		return val
	case []byte:
		return string(val)
	case fmt.Stringer:
		return val.String()
	case nil:
		return ""
	default:
		return fmt.Sprint(val)
	}
}

func asInt64(v interface{}) int64 {
	switch val := v.(type) {
	case int:
		return int64(val)
	case int32:
		return int64(val)
	case int64:
		return val
	case uint:
		return int64(val)
	case uint32:
		return int64(val)
	case uint64:
		return int64(val)
	case float64:
		return int64(val)
	case float32:
		return int64(val)
	case string:
		if val == "" {
			return 0
		}
		var parsed int64
		fmt.Sscan(val, &parsed)
		return parsed
	case nil:
		return 0
	default:
		return 0
	}
}

func asTime(v interface{}) (time.Time, bool) {
	switch val := v.(type) {
	case time.Time:
		return val, true
	case *time.Time:
		if val == nil {
			return time.Time{}, false
		}
		return *val, true
	case string:
		if val == "" {
			return time.Time{}, false
		}
		if ts, err := time.Parse(time.RFC3339Nano, val); err == nil {
			return ts, true
		}
		return time.Time{}, false
	default:
		return time.Time{}, false
	}
}

func countPlaceholders(query, marker string) int {
	lower := strings.ToLower(query)
	idx := strings.Index(lower, marker)
	if idx == -1 {
		return 0
	}
	start := idx + len(marker)
	depth := 1
	count := 0
	for i := start; i < len(query); i++ {
		switch query[i] {
		case '(':
			depth++
		case ')':
			depth--
			if depth == 0 {
				return count
			}
		case '?':
			if depth > 0 {
				count++
			}
		}
	}
	return count
}

func likeMatch(value, pattern string) bool {
	valueRunes := []rune(strings.ToLower(value))
	patternRunes := []rune(strings.ToLower(pattern))
	return matchLikeRunes(valueRunes, patternRunes, 0, 0)
}

func matchLikeRunes(value []rune, pattern []rune, vi, pi int) bool {
	for pi < len(pattern) {
		switch pattern[pi] {
		case '%':
			for pi < len(pattern) && pattern[pi] == '%' {
				pi++
			}
			if pi == len(pattern) {
				return true
			}
			for offset := vi; offset <= len(value); offset++ {
				if matchLikeRunes(value, pattern, offset, pi) {
					return true
				}
			}
			return false
		case '_':
			if vi >= len(value) {
				return false
			}
			vi++
			pi++
		case '\\':
			pi++
			if pi >= len(pattern) {
				return vi < len(value) && value[vi] == '\\' && vi+1 == len(value)
			}
			if vi >= len(value) || value[vi] != pattern[pi] {
				return false
			}
			vi++
			pi++
		default:
			if vi >= len(value) || value[vi] != pattern[pi] {
				return false
			}
			vi++
			pi++
		}
	}
	return vi == len(value)
}

func assignSlice(dest interface{}, rows interface{}) error {
	destVal := reflect.ValueOf(dest)
	if destVal.Kind() != reflect.Ptr || destVal.IsNil() {
		return fmt.Errorf("sqlx: invalid destination")
	}
	sliceVal := destVal.Elem()
	rowsVal := reflect.ValueOf(rows)
	if rowsVal.Kind() == reflect.Ptr {
		if rowsVal.IsNil() {
			sliceVal.Set(reflect.Zero(sliceVal.Type()))
			return nil
		}
		rowsVal = rowsVal.Elem()
	}
	if rowsVal.Kind() != reflect.Slice {
		return fmt.Errorf("sqlx: expected slice rows, got %s", rowsVal.Kind())
	}
	result := reflect.MakeSlice(sliceVal.Type(), rowsVal.Len(), rowsVal.Len())
	for i := 0; i < rowsVal.Len(); i++ {
		elem, err := convertValue(rowsVal.Index(i), sliceVal.Type().Elem())
		if err != nil {
			return err
		}
		result.Index(i).Set(elem)
	}
	sliceVal.Set(result)
	return nil
}

func assignValue(dest interface{}, value interface{}) error {
	destVal := reflect.ValueOf(dest)
	if destVal.Kind() != reflect.Ptr || destVal.IsNil() {
		return fmt.Errorf("sqlx: invalid destination")
	}
	elem, err := convertValue(reflect.ValueOf(value), destVal.Elem().Type())
	if err != nil {
		return err
	}
	destVal.Elem().Set(elem)
	return nil
}

func programRowToStruct(row *programRow) interface{} {
	return struct {
		ID         int64     `db:"id"`
		ProjectID  string    `db:"project_id"`
		Name       string    `db:"name"`
		SourcePath string    `db:"source_path"`
		Summary    string    `db:"summary"`
		CreatedAt  time.Time `db:"created_at"`
		UpdatedAt  time.Time `db:"updated_at"`
	}{
		ID:         row.ID,
		ProjectID:  row.ProjectID,
		Name:       row.Name,
		SourcePath: row.SourcePath,
		Summary:    row.Summary,
		CreatedAt:  row.CreatedAt,
		UpdatedAt:  row.UpdatedAt,
	}
}

func convertValue(src reflect.Value, dstType reflect.Type) (reflect.Value, error) {
	if !src.IsValid() {
		return reflect.Zero(dstType), nil
	}
	if src.Kind() == reflect.Interface && !src.IsNil() {
		src = src.Elem()
	}
	if src.Kind() == reflect.Ptr {
		if src.IsNil() {
			return reflect.Zero(dstType), nil
		}
		src = src.Elem()
	}
	if dstType.Kind() == reflect.Ptr {
		converted, err := convertValue(src, dstType.Elem())
		if err != nil {
			return reflect.Value{}, err
		}
		ptr := reflect.New(dstType.Elem())
		ptr.Elem().Set(converted)
		return ptr, nil
	}
	if src.Type().AssignableTo(dstType) {
		return src.Convert(dstType), nil
	}
	if src.Type().ConvertibleTo(dstType) {
		return src.Convert(dstType), nil
	}
	if dstType.Kind() == reflect.Struct && src.Kind() == reflect.Struct {
		return mapStruct(src, dstType), nil
	}
	if dstType.Kind() == reflect.Interface && src.Type().Implements(dstType) {
		return src, nil
	}
	return reflect.Value{}, fmt.Errorf("sqlx: cannot convert %s to %s", src.Type(), dstType)
}

func mapStruct(src reflect.Value, dstType reflect.Type) reflect.Value {
	dst := reflect.New(dstType).Elem()
	for i := 0; i < dst.NumField(); i++ {
		fieldInfo := dstType.Field(i)
		key := fieldInfo.Tag.Get("db")
		if key == "" {
			key = fieldInfo.Name
		}
		field := findField(src, key)
		if !field.IsValid() {
			continue
		}
		if field.Type().AssignableTo(fieldInfo.Type) {
			dst.Field(i).Set(field.Convert(fieldInfo.Type))
		} else if field.Type().ConvertibleTo(fieldInfo.Type) {
			dst.Field(i).Set(field.Convert(fieldInfo.Type))
		}
	}
	return dst
}

func findField(v reflect.Value, name string) reflect.Value {
	if v.Kind() == reflect.Interface && !v.IsNil() {
		v = v.Elem()
	}
	if v.Kind() == reflect.Ptr {
		if v.IsNil() {
			return reflect.Value{}
		}
		v = v.Elem()
	}
	if v.Kind() != reflect.Struct {
		return reflect.Value{}
	}
	lowered := strings.ToLower(name)
	for i := 0; i < v.NumField(); i++ {
		field := v.Type().Field(i)
		tag := strings.ToLower(field.Tag.Get("db"))
		if tag != "" && tag == lowered {
			return v.Field(i)
		}
		if strings.ToLower(field.Name) == lowered {
			return v.Field(i)
		}
	}
	return reflect.Value{}
}

func In(query string, args ...interface{}) (string, []interface{}, error) {
	if len(args) != 2 {
		return "", nil, fmt.Errorf("sqlx: unsupported In args")
	}
	kinds, ok := args[1].([]string)
	if !ok {
		return "", nil, fmt.Errorf("sqlx: expected []string for In")
	}
	if len(kinds) == 0 {
		query = strings.Replace(query, "(?)", "('')", 1)
		return query, []interface{}{args[0]}, nil
	}
	placeholders := strings.Repeat("?,", len(kinds))
	if len(placeholders) > 0 {
		placeholders = placeholders[:len(placeholders)-1]
	}
	query = strings.Replace(query, "(?)", "("+placeholders+")", 1)
	outArgs := []interface{}{args[0]}
	for _, k := range kinds {
		outArgs = append(outArgs, k)
	}
	return query, outArgs, nil
}
