// File path: internal/graph/kuzu/client.go
package kuzu

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"strings"
	"sync"
	"time"

	"github.com/nicodishanthj/Katral_phase1/internal/common"
	"github.com/nicodishanthj/Katral_phase1/internal/common/telemetry"
	"github.com/nicodishanthj/Katral_phase1/internal/graph"
)

// Client implements graph.Client using the Kuzu REST API with a lightweight
// connection pool to limit concurrent queries.
type Client struct {
	cfg        Config
	httpClient *http.Client
	transport  *http.Transport
	baseURL    string

	pool      chan struct{}
	closing   chan struct{}
	closeOnce sync.Once

	mu        sync.RWMutex
	available bool
}

// Session represents a logical lease from the client's connection pool.
type Session struct {
	client *Client
	once   sync.Once
}

type queryRequest struct {
	Query    string                 `json:"query"`
	Params   map[string]interface{} `json:"params,omitempty"`
	Database string                 `json:"database,omitempty"`
}

type queryResponse struct {
	Error string `json:"error,omitempty"`
}

// NewClient constructs a client from the provided configuration.
func NewClient(ctx context.Context, cfg Config) (*Client, error) {
	if !cfg.Enabled() {
		return nil, errors.New("kuzu disabled")
	}
	logger := common.Logger()
	logger.Info("graph: initializing kuzu client", "endpoint", cfg.Endpoint, "database", cfg.Database, "pool", cfg.MaxConnections, "timeout", cfg.Timeout)

	transport := &http.Transport{
		MaxIdleConns:        cfg.HTTPMaxIdleConns,
		MaxIdleConnsPerHost: cfg.HTTPMaxIdlePerHost,
		MaxConnsPerHost:     cfg.HTTPMaxConnsPerHost,
		IdleConnTimeout:     cfg.HTTPIdleConnTimeout,
	}

	client := &Client{
		cfg:        cfg,
		httpClient: &http.Client{Timeout: cfg.Timeout, Transport: transport},
		transport:  transport,
		baseURL:    strings.TrimRight(cfg.Endpoint, "/"),
		pool:       make(chan struct{}, cfg.MaxConnections),
		closing:    make(chan struct{}),
	}
	for i := 0; i < cfg.MaxConnections; i++ {
		client.pool <- struct{}{}
	}
	client.setAvailable(true)
	if err := client.ping(ctx); err != nil {
		logger.Warn("graph: kuzu ping failed", "error", err)
		client.setAvailable(false)
		return client, nil
	}
	logger.Info("graph: kuzu client ready")
	return client, nil
}

// NewFromEnv loads configuration and constructs a client instance.
func NewFromEnv(ctx context.Context) (*Client, error) {
	cfg, err := LoadConfig()
	if err != nil {
		return nil, err
	}
	if !cfg.Enabled() {
		return nil, nil
	}
	return NewClient(ctx, cfg)
}

// Available reports whether the client is ready for use.
func (c *Client) Available() bool {
	if c == nil {
		return false
	}
	c.mu.RLock()
	defer c.mu.RUnlock()
	return c.available
}

// Connect acquires a session from the pool, respecting context cancellation.
func (c *Client) Connect(ctx context.Context) (*Session, error) {
	return c.acquire(ctx)
}

// Close releases the underlying HTTP client resources.
func (c *Client) Close() error {
	if c == nil {
		return nil
	}
	c.closeOnce.Do(func() {
		close(c.closing)
		c.setAvailable(false)
		if c.transport != nil {
			c.transport.CloseIdleConnections()
		}
	})
	return nil
}

// EnsureSchema ensures that the required graph schema exists.
func (c *Client) EnsureSchema(ctx context.Context) error {
	if c == nil {
		return errors.New("kuzu client not configured")
	}
	statements := []string{
		`CREATE NODE TABLE IF NOT EXISTS Program (
            id STRING,
            name STRING,
            source STRING,
            summary STRING,
            fingerprint STRING,
            technologies STRING[],
            inputs STRING[],
            outputs STRING[],
            paragraphs STRING[],
            updated_at DATETIME,
            PRIMARY KEY (id)
        );`,
		`CREATE NODE TABLE IF NOT EXISTS File (
            id STRING,
            name STRING,
            kind STRING,
            kinds STRING[],
            path STRING,
            summary STRING,
            updated_at DATETIME,
            PRIMARY KEY (id)
        );`,
		`CREATE NODE TABLE IF NOT EXISTS FlowStep (
            id STRING,
            program_id STRING,
            label STRING,
            type STRING,
            sequence INT64,
            next_ids STRING[],
            updated_at DATETIME,
            PRIMARY KEY (id)
        );`,
		`CREATE NODE TABLE IF NOT EXISTS BusinessRule (
            id STRING,
            program_id STRING,
            title STRING,
            summary STRING,
            content STRING,
            tags STRING[],
            updated_at DATETIME,
            PRIMARY KEY (id)
        );`,
		`CREATE NODE TABLE IF NOT EXISTS Paragraph (
            id STRING,
            program_id STRING,
            name STRING,
            summary STRING,
            logic STRING[],
            updated_at DATETIME,
            PRIMARY KEY (id)
        );`,
		`CREATE REL TABLE IF NOT EXISTS CALL (
            FROM Program TO Program,
            type STRING,
            weight DOUBLE,
            occurrences INT64,
            notes STRING,
            updated_at DATETIME,
            PRIMARY KEY (FROM, TO)
        );`,
		`CREATE REL TABLE IF NOT EXISTS INPUT (
            FROM Program TO File,
            weight DOUBLE,
            description STRING,
            updated_at DATETIME,
            PRIMARY KEY (FROM, TO)
        );`,
		`CREATE REL TABLE IF NOT EXISTS OUTPUT (
            FROM Program TO File,
            weight DOUBLE,
            description STRING,
            updated_at DATETIME,
            PRIMARY KEY (FROM, TO)
        );`,
		`CREATE REL TABLE IF NOT EXISTS FLOW (
            FROM Program TO FlowStep,
            weight DOUBLE,
            label STRING,
            updated_at DATETIME,
            PRIMARY KEY (FROM, TO)
        );`,
		`CREATE REL TABLE IF NOT EXISTS BUSINESS_RULES (
            FROM Program TO BusinessRule,
            weight DOUBLE,
            title STRING,
            updated_at DATETIME,
            PRIMARY KEY (FROM, TO)
        );`,
		`CREATE REL TABLE IF NOT EXISTS HasParagraph (
            FROM Program TO Paragraph,
            updated_at DATETIME,
            PRIMARY KEY (FROM, TO)
        );`,
		`CREATE REL TABLE IF NOT EXISTS DataFlow (
            FROM Program TO Program,
            name STRING,
            kind STRING,
            description STRING,
            updated_at DATETIME,
            PRIMARY KEY (FROM, TO, name)
        );`,
	}
	for _, stmt := range statements {
		if err := c.exec(ctx, stmt, nil); err != nil {
			return fmt.Errorf("ensure schema: %w", err)
		}
	}
	return nil
}

// InsertProgram upserts a COBOL program node into the graph.
func (c *Client) InsertProgram(ctx context.Context, program graph.Program) error {
	if program.ID == "" {
		return errors.New("program id required")
	}
	params := map[string]interface{}{
		"id":           program.ID,
		"name":         program.Name,
		"source":       program.SourcePath,
		"summary":      program.Summary,
		"technologies": program.Technologies,
		"inputs":       program.Inputs,
		"outputs":      program.Outputs,
		"paragraphs":   program.Paragraphs,
	}
	query := `MERGE (p:Program {id: $id})
SET p.name = $name,
    p.source = $source,
    p.summary = $summary,
    p.technologies = $technologies,
    p.inputs = $inputs,
    p.outputs = $outputs,
    p.paragraphs = $paragraphs,
    p.updated_at = datetime();`
	return c.exec(ctx, query, params)
}

// InsertParagraph upserts a paragraph node and attaches it to its program.
func (c *Client) InsertParagraph(ctx context.Context, paragraph graph.Paragraph) error {
	if paragraph.ID == "" {
		return errors.New("paragraph id required")
	}
	if paragraph.ProgramID == "" {
		return errors.New("paragraph program id required")
	}
	params := map[string]interface{}{
		"id":         paragraph.ID,
		"program_id": paragraph.ProgramID,
		"name":       paragraph.Name,
		"summary":    paragraph.Summary,
		"logic":      paragraph.Logic,
	}
	query := `MATCH (program:Program {id: $program_id})
MERGE (paragraph:Paragraph {id: $id})
SET paragraph.program_id = $program_id,
    paragraph.name = $name,
    paragraph.summary = $summary,
    paragraph.logic = $logic,
    paragraph.updated_at = datetime()
MERGE (program)-[rel:HasParagraph]->(paragraph)
SET rel.updated_at = datetime();`
	return c.exec(ctx, query, params)
}

// InsertCall upserts an invocation relationship between two programs.
func (c *Client) InsertCall(ctx context.Context, call graph.Call) error {
	if call.From == "" || call.To == "" {
		return errors.New("call endpoints required")
	}
	params := map[string]interface{}{
		"from":        call.From,
		"to":          call.To,
		"type":        call.Type,
		"weight":      float64(call.Occurrences),
		"occurrences": call.Occurrences,
		"notes":       call.Notes,
	}
	query := `MATCH (src:Program {id: $from})
MATCH (dst:Program {id: $to})
MERGE (src)-[rel:CALL]->(dst)
SET rel.type = $type,
    rel.weight = $weight,
    rel.occurrences = $occurrences,
    rel.notes = $notes,
    rel.updated_at = datetime();`
	return c.exec(ctx, query, params)
}

// InsertDataFlow upserts a data flow relationship between two programs.
func (c *Client) InsertDataFlow(ctx context.Context, flow graph.DataFlow) error {
	if flow.From == "" || flow.To == "" {
		return errors.New("data flow endpoints required")
	}
	if strings.TrimSpace(flow.Name) == "" {
		return errors.New("data flow name required")
	}
	params := map[string]interface{}{
		"from":        flow.From,
		"to":          flow.To,
		"name":        flow.Name,
		"kind":        flow.Kind,
		"description": flow.Description,
	}
	query := `MATCH (src:Program {id: $from})
MATCH (dst:Program {id: $to})
MERGE (src)-[rel:DataFlow {name: $name}]->(dst)
SET rel.kind = $kind,
    rel.description = $description,
    rel.updated_at = datetime();`
	return c.exec(ctx, query, params)
}

// ApplyBatch executes the ordered set of statements described by the mutation batch.
func (c *Client) ApplyBatch(ctx context.Context, batch MutationBatch) error {
	if c == nil {
		return errors.New("kuzu client not configured")
	}
	statements := batch.Statements()
	for _, stmt := range statements {
		if err := c.exec(ctx, stmt.Query, stmt.Params); err != nil {
			return err
		}
	}
	return nil
}

// Query executes a Cypher statement using the leased session.
func (s *Session) Query(ctx context.Context, query string, params map[string]interface{}) error {
	if s == nil || s.client == nil {
		return errors.New("session closed")
	}
	return s.client.execute(ctx, query, params)
}

// Close releases the session back to the pool.
func (s *Session) Close() {
	if s == nil || s.client == nil {
		return
	}
	s.once.Do(func() {
		select {
		case s.client.pool <- struct{}{}:
		default:
		}
		s.client = nil
	})
}

func (c *Client) exec(ctx context.Context, query string, params map[string]interface{}) error {
	session, err := c.acquire(ctx)
	if err != nil {
		return err
	}
	defer session.Close()
	return c.execute(ctx, query, params)
}

func (c *Client) execute(ctx context.Context, query string, params map[string]interface{}) error {
	if c == nil {
		return errors.New("kuzu client not configured")
	}
	spanCtx, finish := telemetry.StartSpan(ctx, "graph.kuzu.query")

	start := time.Now()
	statusCode := 0
	defer func() {
		finish("status", statusCode)
	}()

	reqPayload := queryRequest{Query: query, Database: c.cfg.Database}
	if len(params) > 0 {
		reqPayload.Params = params
	}
	buf := &bytes.Buffer{}
	if err := json.NewEncoder(buf).Encode(reqPayload); err != nil {
		return fmt.Errorf("encode query: %w", err)
	}
	request, err := http.NewRequestWithContext(spanCtx, http.MethodPost, c.baseURL+"/query", buf)
	if err != nil {
		return fmt.Errorf("build request: %w", err)
	}
	request.Header.Set("Content-Type", "application/json")
	if c.cfg.Username != "" {
		request.SetBasicAuth(c.cfg.Username, c.cfg.Password)
	}
	resp, err := c.httpClient.Do(request)
	if err != nil {
		c.setAvailable(false)
		telemetry.RecordGraphQuery("kuzu_http", time.Since(start))
		return fmt.Errorf("kuzu request failed: %w", err)
	}
	defer resp.Body.Close()
	statusCode = resp.StatusCode
	if resp.StatusCode >= 400 {
		c.setAvailable(false)
		telemetry.RecordGraphQuery("kuzu_http", time.Since(start))
		return fmt.Errorf("kuzu query failed: status %d", resp.StatusCode)
	}
	var qr queryResponse
	if err := json.NewDecoder(resp.Body).Decode(&qr); err != nil && !errors.Is(err, context.Canceled) {
		telemetry.RecordGraphQuery("kuzu_http", time.Since(start))
		return fmt.Errorf("decode kuzu response: %w", err)
	}
	if strings.TrimSpace(qr.Error) != "" {
		telemetry.RecordGraphQuery("kuzu_http", time.Since(start))
		return errors.New(qr.Error)
	}
	c.setAvailable(true)
	telemetry.RecordGraphQuery("kuzu_http", time.Since(start))
	finish("status", statusCode)
	return nil
}

func (c *Client) ping(ctx context.Context) error {
	pingTimeout := c.cfg.Timeout
	if pingTimeout <= 0 {
		pingTimeout = 5 * time.Second
	}
	const minPingTimeout = 500 * time.Millisecond
	if pingTimeout < minPingTimeout {
		pingTimeout = minPingTimeout
	}
	ctx, cancel := context.WithTimeout(ctx, pingTimeout)
	defer cancel()
	return c.exec(ctx, "RETURN 1;", nil)
}

func (c *Client) setAvailable(ready bool) {
	c.mu.Lock()
	c.available = ready
	c.mu.Unlock()
}

func (c *Client) acquire(ctx context.Context) (*Session, error) {
	if c == nil {
		return nil, errors.New("kuzu client not configured")
	}
	if !c.Available() {
		return nil, errors.New("kuzu unavailable")
	}
	select {
	case <-ctx.Done():
		return nil, ctx.Err()
	case <-c.closing:
		return nil, errors.New("kuzu client closed")
	case <-c.pool:
		return &Session{client: c}, nil
	}
}
