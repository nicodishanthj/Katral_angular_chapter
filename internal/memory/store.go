// File path: internal/memory/store.go
package memory

import (
	"bufio"
	"context"
	"encoding/base64"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"sync"

	"github.com/nicodishanthj/Katral_phase1/internal/kb"
)

type Store struct {
	path string
	mu   sync.RWMutex
}

func NewStore(path string) (*Store, error) {
	if path == "" {
		return nil, errors.New("store path required")
	}
	basePath := determineRoot(path)
	if err := os.MkdirAll(basePath, 0o755); err != nil {
		return nil, fmt.Errorf("create store dir: %w", err)
	}
	return &Store{path: basePath}, nil
}

func (s *Store) AppendDocs(ctx context.Context, projectID string, docs []kb.Doc) error {
	if len(docs) == 0 {
		return nil
	}
	filePath, err := s.projectFile(projectID)
	if err != nil {
		return err
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	file, err := os.OpenFile(filePath, os.O_APPEND|os.O_WRONLY|os.O_CREATE, 0o644)
	if err != nil {
		return fmt.Errorf("open store: %w", err)
	}
	defer file.Close()
	enc := json.NewEncoder(file)
	for _, doc := range docs {
		select {
		case <-ctx.Done():
			return ctx.Err()
		default:
		}
		if err := enc.Encode(doc); err != nil {
			return fmt.Errorf("encode doc: %w", err)
		}
	}
	return nil
}

// ReplaceDocs overwrites the existing store contents with the provided documents for a project.
func (s *Store) ReplaceDocs(ctx context.Context, projectID string, docs []kb.Doc) error {
	filePath, err := s.projectFile(projectID)
	if err != nil {
		return err
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	file, err := os.OpenFile(filePath, os.O_TRUNC|os.O_WRONLY|os.O_CREATE, 0o644)
	if err != nil {
		return fmt.Errorf("open store: %w", err)
	}
	defer file.Close()
	if len(docs) == 0 {
		return nil
	}
	enc := json.NewEncoder(file)
	for _, doc := range docs {
		select {
		case <-ctx.Done():
			return ctx.Err()
		default:
		}
		if err := enc.Encode(doc); err != nil {
			return fmt.Errorf("encode doc: %w", err)
		}
	}
	return nil
}

func (s *Store) AllDocs(ctx context.Context, projectID string) ([]kb.Doc, error) {
	if s == nil {
		return nil, errors.New("store not initialized")
	}
	s.mu.RLock()
	defer s.mu.RUnlock()
	if strings.TrimSpace(projectID) == "" {
		return s.readAllProjects(ctx)
	}
	return s.readProject(ctx, projectID)
}

// Projects returns metadata about stored projects including their document counts.
func (s *Store) Projects(ctx context.Context) ([]ProjectInfo, error) {
	if s == nil {
		return nil, errors.New("store not initialized")
	}
	s.mu.RLock()
	defer s.mu.RUnlock()
	entries, err := os.ReadDir(s.path)
	if err != nil {
		return nil, fmt.Errorf("read store dir: %w", err)
	}
	infos := make([]ProjectInfo, 0, len(entries))
	for _, entry := range entries {
		if entry.IsDir() {
			continue
		}
		projectID, ok := decodeProjectFile(entry.Name())
		if !ok {
			continue
		}
		docs, err := s.readProject(ctx, projectID)
		if err != nil {
			return nil, err
		}
		infos = append(infos, ProjectInfo{ID: projectID, Documents: len(docs)})
	}
	sort.Slice(infos, func(i, j int) bool {
		return infos[i].ID < infos[j].ID
	})
	return infos, nil
}

// Root returns the underlying directory used for persistence.
func (s *Store) Root() string {
	if s == nil {
		return ""
	}
	return s.path
}

type ProjectInfo struct {
	ID        string
	Documents int
}

func (s *Store) readProject(ctx context.Context, projectID string) ([]kb.Doc, error) {
	filePath, err := s.projectFile(projectID)
	if err != nil {
		return nil, err
	}
	file, err := os.Open(filePath)
	if err != nil {
		if errors.Is(err, os.ErrNotExist) {
			return nil, nil
		}
		return nil, fmt.Errorf("open store: %w", err)
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)
	scanner.Buffer(make([]byte, 64<<10), 8<<20)
	var docs []kb.Doc
	for scanner.Scan() {
		select {
		case <-ctx.Done():
			return nil, ctx.Err()
		default:
		}
		line := scanner.Bytes()
		if len(line) == 0 {
			continue
		}
		var doc kb.Doc
		if err := json.Unmarshal(line, &doc); err != nil {
			return nil, fmt.Errorf("decode doc: %w", err)
		}
		docs = append(docs, doc)
	}
	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("scan docs: %w", err)
	}
	return docs, nil
}

func (s *Store) readAllProjects(ctx context.Context) ([]kb.Doc, error) {
	entries, err := os.ReadDir(s.path)
	if err != nil {
		if errors.Is(err, os.ErrNotExist) {
			return nil, nil
		}
		return nil, fmt.Errorf("read store dir: %w", err)
	}
	var all []kb.Doc
	for _, entry := range entries {
		if entry.IsDir() {
			continue
		}
		projectID, ok := decodeProjectFile(entry.Name())
		if !ok {
			continue
		}
		docs, err := s.readProject(ctx, projectID)
		if err != nil {
			return nil, err
		}
		all = append(all, docs...)
	}
	return all, nil
}

func (s *Store) projectFile(projectID string) (string, error) {
	trimmed := strings.TrimSpace(projectID)
	if trimmed == "" {
		return "", fmt.Errorf("project id required")
	}
	encoded := base64.RawURLEncoding.EncodeToString([]byte(trimmed))
	if encoded == "" {
		return "", fmt.Errorf("invalid project id")
	}
	name := fmt.Sprintf("project_%s.jsonl", encoded)
	return filepath.Join(s.path, name), nil
}

func decodeProjectFile(name string) (string, bool) {
	if !strings.HasPrefix(name, "project_") || !strings.HasSuffix(name, ".jsonl") {
		return "", false
	}
	encoded := strings.TrimSuffix(strings.TrimPrefix(name, "project_"), ".jsonl")
	data, err := base64.RawURLEncoding.DecodeString(encoded)
	if err != nil {
		return "", false
	}
	return string(data), true
}

func determineRoot(path string) string {
	trimmed := strings.TrimSpace(path)
	if trimmed == "" {
		return "."
	}
	info, err := os.Stat(trimmed)
	if err == nil {
		if info.IsDir() {
			return trimmed
		}
		return filepath.Dir(trimmed)
	}
	if !errors.Is(err, fs.ErrNotExist) {
		return filepath.Dir(trimmed)
	}
	// Path does not exist; assume caller intended a file if an extension is present.
	if ext := filepath.Ext(trimmed); ext != "" {
		dir := filepath.Dir(trimmed)
		if dir == "" || dir == "." {
			return "."
		}
		return dir
	}
	return trimmed
}
