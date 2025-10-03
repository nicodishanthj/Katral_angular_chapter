// File path: internal/workflow/manager.go
package workflow

import (
	"archive/zip"
	"context"
	"encoding/base64"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/nicodishanthj/Katral_phase1/internal/common"
	ctxbuilder "github.com/nicodishanthj/Katral_phase1/internal/context"
	"github.com/nicodishanthj/Katral_phase1/internal/graph"
	"github.com/nicodishanthj/Katral_phase1/internal/kb"
	"github.com/nicodishanthj/Katral_phase1/internal/llm"
	"github.com/nicodishanthj/Katral_phase1/internal/memory"
	"github.com/nicodishanthj/Katral_phase1/internal/metadata"
	"github.com/nicodishanthj/Katral_phase1/internal/retriever"
	"github.com/nicodishanthj/Katral_phase1/internal/vector"
)

const maxLogEntries = 500

var (
	ErrWorkflowRunning        = errors.New("workflow already running")
	ErrWorkflowNotFound       = errors.New("workflow not found")
	ErrWorkflowNotRunning     = errors.New("workflow not running")
	ErrArtifactNotFound       = errors.New("artifact not available")
	ErrArtifactInvalid        = errors.New("artifact invalid")
	ErrSpringArtifactNotFound = ErrArtifactNotFound
	ErrSpringArtifactInvalid  = ErrArtifactInvalid
)

type Kind string

const (
	KindCodeConversion     Kind = "code-conversion"
	KindKnowledgeBase      Kind = "knowledge-base"
	KindDocumentGeneration Kind = "doc-generation"
)

type StepStatus string

const (
	StepPending   StepStatus = "pending"
	StepRunning   StepStatus = "running"
	StepCompleted StepStatus = "completed"
	StepSkipped   StepStatus = "skipped"
	StepError     StepStatus = "error"
)

type LogEntry struct {
	Time    time.Time `json:"time"`
	Level   string    `json:"level"`
	Message string    `json:"message"`
}

type Step struct {
	Name        string     `json:"name"`
	Status      StepStatus `json:"status"`
	Message     string     `json:"message,omitempty"`
	StartedAt   *time.Time `json:"started_at,omitempty"`
	CompletedAt *time.Time `json:"completed_at,omitempty"`
}

type Reference struct {
	Program string                  `json:"program"`
	Source  string                  `json:"source"`
	Score   float64                 `json:"score"`
	Summary string                  `json:"summary,omitempty"`
	Graph   ctxbuilder.GraphContext `json:"graph,omitempty"`
}

type KnowledgeConfig struct {
	Repo       string   `json:"repo"`
	Stacks     []string `json:"stacks"`
	Collection string   `json:"collection"`
}

type TargetConfig struct {
	Language  string `json:"language"`
	Version   string `json:"version"`
	Framework string `json:"framework"`
	Runtime   string `json:"runtime"`
	Notes     string `json:"notes"`
}

type MigrationConfig struct {
	AngularSourceRoot          string            `json:"angular_source_root"`
	AngularVersion             string            `json:"angular_version"`
	ReactTargetRoot            string            `json:"react_target_root"`
	ReactVersion               string            `json:"react_version"`
	PatternMapping             map[string]string `json:"pattern_mapping,omitempty"`
	ComponentLifecycleMapping  map[string]string `json:"component_lifecycle_mapping,omitempty"`
	DirectiveConversionMapping map[string]string `json:"directive_conversion_mapping,omitempty"`
	ServiceContextMapping      map[string]string `json:"service_context_mapping,omitempty"`
	PipeConversionMapping      map[string]string `json:"pipe_conversion_mapping,omitempty"`
	GuardRouteMapping          map[string]string `json:"guard_route_mapping,omitempty"`
}

type Request struct {
	ProjectID        string           `json:"project_id"`
	Mainframe        string           `json:"mainframe"`
	Stacks           []string         `json:"stacks"`
	Collection       string           `json:"collection"`
	GeneratorCommand string           `json:"generator"`
	GeneratorDir     string           `json:"generator_dir"`
	SpringProject    string           `json:"spring"`
	ReviewPrompt     string           `json:"review"`
	ResetStore       bool             `json:"reset_store"`
	VerifySpring     bool             `json:"verify_spring"`
	Flow             string           `json:"flow"`
	KnowledgeConfig  *KnowledgeConfig `json:"knowledge_config,omitempty"`
	TargetConfig     *TargetConfig    `json:"target_config,omitempty"`
	MigrationConfig  *MigrationConfig `json:"migration_config,omitempty"`

	kind Kind
}

type State struct {
	Status                 string            `json:"status"`
	Running                bool              `json:"running"`
	StartedAt              *time.Time        `json:"started_at,omitempty"`
	CompletedAt            *time.Time        `json:"completed_at,omitempty"`
	Steps                  []Step            `json:"steps"`
	Error                  string            `json:"error,omitempty"`
	ReviewAnswer           string            `json:"review_answer,omitempty"`
	ReviewReferences       []Reference       `json:"review_references,omitempty"`
	SpringArtifact         string            `json:"spring_artifact,omitempty"`
	ConversionArtifacts    map[string]string `json:"conversion_artifacts,omitempty"`
	ConsolidatedDoc        string            `json:"consolidated_doc,omitempty"`
	DocumentationArtifacts map[string]string `json:"documentation_artifacts,omitempty"`
	Request                Request           `json:"request"`
}

// DocumentationRenderedDoc represents a markdown payload prepared for the UI.
type DocumentationRenderedDoc struct {
	ID       string `json:"id"`
	Summary  string `json:"summary,omitempty"`
	Markdown string `json:"markdown"`
	Source   string `json:"source_path,omitempty"`
	Type     string `json:"type,omitempty"`
}

// DocumentationProgramGroup describes the rendered documents for a program.
type DocumentationProgramGroup struct {
	Program   string                     `json:"program"`
	Documents []DocumentationRenderedDoc `json:"documents"`
}

// DocumentationArtifactSet contains rendered documentation grouped by type.
type DocumentationArtifactSet map[string][]DocumentationProgramGroup

type session struct {
	state  State
	cancel context.CancelFunc
}

type Manager struct {
	store     *memory.Store
	provider  llm.Provider
	retriever *retriever.Retriever
	ctx       ctxbuilder.Builder
	vector    vector.Store
	graph     graph.Client
	graphSvc  graph.DependencyService
	metadata  metadata.Store

	rewriteDocTypes map[string]struct{}
	embedDocTypes   map[string]struct{}

	historyPath string
	historyMu   sync.Mutex
	history     map[string]State

	logMu sync.Mutex
	logs  []LogEntry

	workflowMu sync.Mutex
	workflows  map[string]*session

	artifactRoot string

	ingestBatchSize int
}

func NewManager(store *memory.Store, provider llm.Provider, retr *retriever.Retriever, ctxBuilder ctxbuilder.Builder, vectorStore vector.Store, graphClient graph.Client, depSvc graph.DependencyService, metadataStore metadata.Store, rewriteDocTypes, embedDocTypes map[string]struct{}) *Manager {
	mgr := &Manager{
		store:           store,
		provider:        provider,
		retriever:       retr,
		ctx:             ctxBuilder,
		vector:          vectorStore,
		graph:           graphClient,
		graphSvc:        depSvc,
		metadata:        metadataStore,
		rewriteDocTypes: rewriteDocTypes,
		embedDocTypes:   embedDocTypes,
		logs:            make([]LogEntry, 0, 32),
		workflows:       make(map[string]*session),
		history:         make(map[string]State),
	}
	if store != nil {
		mgr.historyPath = filepath.Join(store.Root(), "projects_history.json")
		mgr.artifactRoot = filepath.Join(store.Root(), "artifacts")
	} else {
		mgr.artifactRoot = filepath.Join(os.TempDir(), "katral_artifacts")
	}
	if mgr.artifactRoot != "" {
		if err := os.MkdirAll(mgr.artifactRoot, 0o755); err != nil {
			common.Logger().Warn("workflow: create artifact root failed", "error", err, "path", mgr.artifactRoot)
			mgr.artifactRoot = ""
		}
	}
	if err := mgr.loadHistory(); err != nil {
		common.Logger().Warn("workflow: load history failed", "error", err)
	}
	mgr.ingestBatchSize = loadIngestBatchSize()
	return mgr
}

func loadIngestBatchSize() int {
	value := strings.TrimSpace(os.Getenv("KATRAL_INGEST_BATCH_SIZE"))
	if value == "" {
		return 64
	}
	parsed, err := strconv.Atoi(value)
	if err != nil {
		common.Logger().Warn("workflow: invalid ingest batch size", "value", value, "error", err)
		return 64
	}
	if parsed <= 0 {
		return 64
	}
	return parsed
}

// MetadataStore exposes the metadata catalog backing this workflow manager.
func (m *Manager) MetadataStore() metadata.Store {
	return m.metadata
}

// TechnologyUsage returns aggregated technology metrics for a project when the
// metadata store is available.
func (m *Manager) TechnologyUsage(ctx context.Context, projectID string) ([]metadata.TechnologyUsage, error) {
	if m.metadata == nil {
		return nil, errors.New("metadata store unavailable")
	}
	return m.metadata.TechnologyUsage(ctx, projectID)
}

func (m *Manager) AppendLog(level, format string, args ...interface{}) {
	text := fmt.Sprintf(format, args...)
	entry := LogEntry{Time: time.Now().UTC(), Level: level, Message: text}
	m.logMu.Lock()
	m.logs = append(m.logs, entry)
	if len(m.logs) > maxLogEntries {
		m.logs = m.logs[len(m.logs)-maxLogEntries:]
	}
	m.logMu.Unlock()
	logger := common.Logger()
	switch level {
	case "error":
		logger.Error(text)
	case "warn":
		logger.Warn(text)
	case "debug":
		logger.Debug(text)
	default:
		logger.Info(text)
	}
}

func (m *Manager) Logs() []LogEntry {
	m.logMu.Lock()
	defer m.logMu.Unlock()
	entries := make([]LogEntry, len(m.logs))
	copy(entries, m.logs)
	return entries
}

func (m *Manager) Start(req Request) error {
	normalized, err := normalizeRequest(req)
	if err != nil {
		return err
	}
	steps := buildWorkflowSteps(normalized.kind)
	if len(steps) == 0 {
		return fmt.Errorf("no steps configured for workflow %s", normalized.Flow)
	}
	now := time.Now().UTC()
	state := State{
		Status:      "running",
		Running:     true,
		StartedAt:   &now,
		CompletedAt: nil,
		Steps:       steps,
		Error:       "",
		Request:     normalized,
	}
	ctx, cancel := context.WithCancel(context.Background())
	m.workflowMu.Lock()
	if existing, ok := m.workflows[normalized.ProjectID]; ok && existing.state.Running {
		m.workflowMu.Unlock()
		cancel()
		return ErrWorkflowRunning
	}
	m.workflows[normalized.ProjectID] = &session{state: state, cancel: cancel}
	m.workflowMu.Unlock()
	go m.runWorkflow(ctx, normalized.ProjectID, normalized)
	if normalized.Flow != "" {
		m.AppendLog("info", "Workflow started (%s) for project %s targeting %s", normalized.Flow, normalized.ProjectID, normalized.Mainframe)
	} else {
		m.AppendLog("info", "Workflow started for project %s targeting %s", normalized.ProjectID, normalized.Mainframe)
	}
	return nil
}

func (m *Manager) Stop(projectID string) error {
	projectID = strings.TrimSpace(projectID)
	if projectID == "" {
		return fmt.Errorf("project id required")
	}
	m.workflowMu.Lock()
	session, ok := m.workflows[projectID]
	if !ok {
		m.workflowMu.Unlock()
		return ErrWorkflowNotFound
	}
	if !session.state.Running || session.cancel == nil {
		m.workflowMu.Unlock()
		return ErrWorkflowNotRunning
	}
	if session.state.Status != "canceling" {
		session.state.Status = "canceling"
	}
	cancel := session.cancel
	m.workflowMu.Unlock()
	cancel()
	m.AppendLog("info", "Cancellation requested for project %s", projectID)
	return nil
}

func (m *Manager) Status(projectID string) State {
	projectID = strings.TrimSpace(projectID)
	if projectID == "" {
		return newState()
	}

	m.workflowMu.Lock()
	session, ok := m.workflows[projectID]
	if ok {
		snapshot := cloneState(session.state)
		m.workflowMu.Unlock()
		return snapshot
	}
	m.workflowMu.Unlock()

	m.historyMu.Lock()
	historyState, ok := m.history[projectID]
	if ok {
		snapshot := cloneState(historyState)
		m.historyMu.Unlock()
		return snapshot
	}
	m.historyMu.Unlock()

	result := newState()
	result.Request.ProjectID = projectID
	return result
}

func (m *Manager) SpringArtifactPath(projectID string) (string, error) {
	projectID = strings.TrimSpace(projectID)
	if projectID == "" {
		return "", fmt.Errorf("project id required")
	}
	state := m.Status(projectID)
	artifact := strings.TrimSpace(state.SpringArtifact)
	if artifact == "" {
		return "", ErrArtifactNotFound
	}
	return m.validateArtifactPath(artifact)
}

func (m *Manager) ConsolidatedDocPath(projectID string) (string, error) {
	projectID = strings.TrimSpace(projectID)
	if projectID == "" {
		return "", fmt.Errorf("project id required")
	}
	state := m.Status(projectID)
	artifact := strings.TrimSpace(state.ConsolidatedDoc)
	if artifact == "" {
		return "", ErrArtifactNotFound
	}
	return m.validateArtifactPath(artifact)
}

func (m *Manager) DocumentationArtifactPath(projectID, docType string) (string, error) {
	projectID = strings.TrimSpace(projectID)
	if projectID == "" {
		return "", fmt.Errorf("project id required")
	}
	typeKey := strings.ToLower(strings.TrimSpace(docType))
	if typeKey == "" {
		return "", fmt.Errorf("documentation artifact type required")
	}
	state := m.Status(projectID)
	var artifact string
	if len(state.DocumentationArtifacts) > 0 {
		for key, value := range state.DocumentationArtifacts {
			if strings.EqualFold(strings.TrimSpace(key), typeKey) {
				artifact = strings.TrimSpace(value)
				break
			}
		}
	}
	if artifact == "" {
		return "", ErrArtifactNotFound
	}
	return m.validateArtifactPath(artifact)
}

// LoadDocumentationArtifacts loads documentation archives for the provided
// project and prepares markdown payloads grouped by artifact type and program.
func (m *Manager) LoadDocumentationArtifacts(projectID string) (DocumentationArtifactSet, error) {
	projectID = strings.TrimSpace(projectID)
	if projectID == "" {
		return nil, fmt.Errorf("project id required")
	}
	state := m.Status(projectID)
	if len(state.DocumentationArtifacts) == 0 {
		return DocumentationArtifactSet{}, nil
	}
	rendered := make(DocumentationArtifactSet, len(state.DocumentationArtifacts))
	for kind := range state.DocumentationArtifacts {
		path, err := m.DocumentationArtifactPath(projectID, kind)
		if err != nil {
			return nil, err
		}
		docs, err := loadDocumentationArchive(path)
		if err != nil {
			return nil, fmt.Errorf("load %s artifact: %w", kind, err)
		}
		if len(docs) == 0 {
			continue
		}
		programIndex := make(map[string]*DocumentationProgramGroup)
		order := make([]string, 0, len(docs))
		for _, doc := range docs {
			markdown := documentationMarkdownFromDoc(doc)
			if strings.TrimSpace(markdown) == "" {
				continue
			}
			program := strings.TrimSpace(doc.Program)
			if program == "" {
				program = "Project"
			}
			group, ok := programIndex[program]
			if !ok {
				group = &DocumentationProgramGroup{Program: program, Documents: make([]DocumentationRenderedDoc, 0, 4)}
				programIndex[program] = group
				order = append(order, program)
			}
			entry := DocumentationRenderedDoc{
				ID:       strings.TrimSpace(doc.ID),
				Summary:  strings.TrimSpace(doc.Summary),
				Markdown: markdown,
				Source:   strings.TrimSpace(doc.SourcePath),
				Type:     strings.TrimSpace(doc.Type),
			}
			group.Documents = append(group.Documents, entry)
		}
		if len(order) == 0 {
			continue
		}
		groups := make([]DocumentationProgramGroup, 0, len(order))
		for _, program := range order {
			if group := programIndex[program]; group != nil && len(group.Documents) > 0 {
				groups = append(groups, *group)
			}
		}
		if len(groups) > 0 {
			rendered[kind] = groups
		}
	}
	return rendered, nil
}

func loadDocumentationArchive(path string) ([]kb.Doc, error) {
	archive, err := zip.OpenReader(path)
	if err != nil {
		return nil, fmt.Errorf("open archive: %w", err)
	}
	defer archive.Close()
	var docs []kb.Doc
	for _, file := range archive.File {
		if file.FileInfo().IsDir() {
			continue
		}
		name := strings.TrimSpace(file.Name)
		if name == "" || strings.EqualFold(name, "manifest.json") || !strings.HasSuffix(strings.ToLower(name), ".json") {
			continue
		}
		reader, err := file.Open()
		if err != nil {
			return nil, fmt.Errorf("open %s: %w", file.Name, err)
		}
		data, err := io.ReadAll(reader)
		reader.Close()
		if err != nil {
			return nil, fmt.Errorf("read %s: %w", file.Name, err)
		}
		var doc kb.Doc
		if err := json.Unmarshal(data, &doc); err != nil {
			return nil, fmt.Errorf("unmarshal %s: %w", file.Name, err)
		}
		docs = append(docs, doc)
	}
	return docs, nil
}

func documentationMarkdownFromDoc(doc kb.Doc) string {
	if doc.Extra != nil {
		if markdown, ok := doc.Extra["documentation_markdown"]; ok {
			if trimmed := strings.TrimSpace(markdown); trimmed != "" {
				return trimmed
			}
		}
	}
	if trimmed := strings.TrimSpace(doc.Content); trimmed != "" {
		return trimmed
	}
	if trimmed := strings.TrimSpace(doc.Summary); trimmed != "" {
		return trimmed
	}
	return ""
}

func (m *Manager) ConversionArtifactPath(projectID, docType string) (string, error) {
	projectID = strings.TrimSpace(projectID)
	if projectID == "" {
		return "", fmt.Errorf("project id required")
	}
	typeKey := strings.ToLower(strings.TrimSpace(docType))
	if typeKey == "" {
		return "", fmt.Errorf("conversion artifact type required")
	}
	state := m.Status(projectID)
	var artifact string
	if len(state.ConversionArtifacts) > 0 {
		for key, value := range state.ConversionArtifacts {
			if strings.EqualFold(strings.TrimSpace(key), typeKey) {
				artifact = strings.TrimSpace(value)
				break
			}
		}
	}
	if artifact == "" {
		return "", ErrArtifactNotFound
	}
	return m.validateArtifactPath(artifact)
}

func (m *Manager) validateArtifactPath(path string) (string, error) {
	absPath, err := filepath.Abs(strings.TrimSpace(path))
	if err != nil {
		return "", fmt.Errorf("resolve artifact path: %w", err)
	}
	root := strings.TrimSpace(m.artifactRoot)
	if root != "" {
		rootAbs, err := filepath.Abs(root)
		if err != nil {
			return "", fmt.Errorf("resolve artifact root: %w", err)
		}
		rel, err := filepath.Rel(rootAbs, absPath)
		if err != nil {
			return "", fmt.Errorf("resolve artifact path: %w", err)
		}
		if rel == ".." || strings.HasPrefix(rel, ".."+string(os.PathSeparator)) {
			return "", ErrArtifactInvalid
		}
	}
	info, err := os.Stat(absPath)
	if err != nil {
		return "", fmt.Errorf("stat artifact: %w", err)
	}
	if info.IsDir() {
		return "", ErrArtifactInvalid
	}
	return absPath, nil
}

func newState() State {
	return State{Status: "idle", Steps: []Step{}}
}

func cloneState(src State) State {
	clone := src
	if len(src.Steps) > 0 {
		clone.Steps = append([]Step(nil), src.Steps...)
	}
	if len(src.ReviewReferences) > 0 {
		clone.ReviewReferences = append([]Reference(nil), src.ReviewReferences...)
	}
	if len(src.ConversionArtifacts) > 0 {
		clone.ConversionArtifacts = make(map[string]string, len(src.ConversionArtifacts))
		for key, value := range src.ConversionArtifacts {
			clone.ConversionArtifacts[key] = value
		}
	}
	if len(src.DocumentationArtifacts) > 0 {
		clone.DocumentationArtifacts = make(map[string]string, len(src.DocumentationArtifacts))
		for key, value := range src.DocumentationArtifacts {
			clone.DocumentationArtifacts[key] = value
		}
	}
	clone.Request.Stacks = append([]string(nil), src.Request.Stacks...)
	if src.Request.KnowledgeConfig != nil {
		cfg := *src.Request.KnowledgeConfig
		cfg.Stacks = append([]string(nil), cfg.Stacks...)
		clone.Request.KnowledgeConfig = &cfg
	}
	if src.Request.TargetConfig != nil {
		cfg := *src.Request.TargetConfig
		clone.Request.TargetConfig = &cfg
	}
	if src.Request.MigrationConfig != nil {
		cfg := *src.Request.MigrationConfig
		if len(cfg.PatternMapping) > 0 {
			cfg.PatternMapping = copyStringMap(cfg.PatternMapping)
		}
		if len(cfg.ComponentLifecycleMapping) > 0 {
			cfg.ComponentLifecycleMapping = copyStringMap(cfg.ComponentLifecycleMapping)
		}
		if len(cfg.DirectiveConversionMapping) > 0 {
			cfg.DirectiveConversionMapping = copyStringMap(cfg.DirectiveConversionMapping)
		}
		if len(cfg.ServiceContextMapping) > 0 {
			cfg.ServiceContextMapping = copyStringMap(cfg.ServiceContextMapping)
		}
		if len(cfg.PipeConversionMapping) > 0 {
			cfg.PipeConversionMapping = copyStringMap(cfg.PipeConversionMapping)
		}
		if len(cfg.GuardRouteMapping) > 0 {
			cfg.GuardRouteMapping = copyStringMap(cfg.GuardRouteMapping)
		}
		clone.Request.MigrationConfig = &cfg
	}
	return clone
}

func copyStringMap(src map[string]string) map[string]string {
	if len(src) == 0 {
		return nil
	}
	dst := make(map[string]string, len(src))
	for key, value := range src {
		dst[key] = value
	}
	return dst
}

func (m *Manager) loadHistory() error {
	if m.historyPath == "" {
		return nil
	}
	data, err := os.ReadFile(m.historyPath)
	if err != nil {
		if errors.Is(err, os.ErrNotExist) {
			return nil
		}
		return err
	}
	if len(data) == 0 {
		return nil
	}
	var stored map[string]State
	if err := json.Unmarshal(data, &stored); err != nil {
		return err
	}
	for id, state := range stored {
		trimmed := strings.TrimSpace(id)
		if trimmed == "" {
			continue
		}
		snapshot := cloneState(state)
		if snapshot.Request.ProjectID == "" {
			snapshot.Request.ProjectID = trimmed
		}
		m.history[trimmed] = snapshot
	}
	return nil
}

func (m *Manager) saveHistoryLocked() error {
	if m.historyPath == "" {
		return nil
	}
	tmpPath := m.historyPath + ".tmp"
	file, err := os.OpenFile(tmpPath, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0o644)
	if err != nil {
		return err
	}
	enc := json.NewEncoder(file)
	if err := enc.Encode(m.history); err != nil {
		_ = file.Close()
		_ = os.Remove(tmpPath)
		return err
	}
	if err := file.Close(); err != nil {
		_ = os.Remove(tmpPath)
		return err
	}
	return os.Rename(tmpPath, m.historyPath)
}

func (m *Manager) persistProjectState(projectID string, state State) {
	projectID = strings.TrimSpace(projectID)
	if projectID == "" {
		return
	}
	snapshot := cloneState(state)
	if snapshot.Request.ProjectID == "" {
		snapshot.Request.ProjectID = projectID
	}
	m.historyMu.Lock()
	if m.history == nil {
		m.history = make(map[string]State)
	}
	m.history[projectID] = snapshot
	if err := m.saveHistoryLocked(); err != nil {
		common.Logger().Warn("workflow: save history failed", "error", err)
	}
	m.historyMu.Unlock()
}

func (m *Manager) ProjectStates() map[string]State {
	result := make(map[string]State)
	m.historyMu.Lock()
	for id, state := range m.history {
		result[id] = cloneState(state)
	}
	m.historyMu.Unlock()
	m.workflowMu.Lock()
	for id, session := range m.workflows {
		result[id] = cloneState(session.state)
	}
	m.workflowMu.Unlock()
	return result
}

func (m *Manager) collectionForProject(projectID, requested string) string {
	return ProjectCollectionName(projectID, requested)
}

func sanitizeCollectionName(name string) string {
	trimmed := strings.TrimSpace(name)
	if trimmed == "" {
		return "default"
	}
	var builder strings.Builder
	for _, r := range trimmed {
		switch {
		case r >= 'a' && r <= 'z':
			builder.WriteRune(r)
		case r >= 'A' && r <= 'Z':
			builder.WriteRune(r + 32)
		case r >= '0' && r <= '9':
			builder.WriteRune(r)
		case r == '-' || r == '_':
			builder.WriteRune('-')
		default:
			builder.WriteRune('-')
		}
	}
	sanitized := strings.Trim(builder.String(), "-_")
	if sanitized == "" {
		return "default"
	}
	if len(sanitized) > 64 {
		sanitized = sanitized[:64]
	}
	return sanitized
}

func ProjectCollectionName(projectID, requested string) string {
	projectID = strings.TrimSpace(projectID)
	if projectID == "" {
		return strings.TrimSpace(requested)
	}
	encoded := base64.RawURLEncoding.EncodeToString([]byte(projectID))
	base := sanitizeCollectionName(requested)
	return fmt.Sprintf("proj_%s__%s", encoded, base)
}
