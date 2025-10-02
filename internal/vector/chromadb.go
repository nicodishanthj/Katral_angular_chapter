// File path: internal/vector/chromadb.go
package vector

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"strings"
	"sync"
	"time"

	"github.com/nicodishanthj/Katral_phase1/internal/common"
	"github.com/nicodishanthj/Katral_phase1/internal/common/telemetry"
	"github.com/nicodishanthj/Katral_phase1/internal/kb"
)

type Store interface {
	Available() bool
	SetCollection(name string)
	Collection() string
	EnsureCollection(ctx context.Context, dim int) error
	UpsertDocs(ctx context.Context, docs []kb.Doc, vectors [][]float32) error
	Search(ctx context.Context, vector []float32, limit int) ([]SearchResult, error)
}

type Client struct {
	httpClient *http.Client
	transport  *http.Transport

	baseURL      string
	collection   string
	collectionID string
	available    bool
	apiKey       string

	cfg Config

	mu sync.RWMutex
}

type SearchResult struct {
	ID      string
	Score   float32
	Payload map[string]interface{}
}

func NewFromEnv(ctx context.Context) (*Client, error) {
	cfg, err := LoadConfig()
	if err != nil {
		return nil, err
	}
	return New(ctx, cfg)
}

// New constructs a client using the provided configuration.
func New(ctx context.Context, cfg Config) (*Client, error) {
	baseURL := fmt.Sprintf("%s://%s:%s/api/v1", cfg.Scheme, cfg.Host, cfg.Port)
	logger := common.Logger()
	logger.Info(
		"vector: initializing chromadb client",
		"host", cfg.Host,
		"port", cfg.Port,
		"collection", cfg.Collection,
		"timeout", cfg.Timeout,
	)

	transport := &http.Transport{
		MaxIdleConns:        cfg.HTTPMaxIdleConns,
		MaxIdleConnsPerHost: cfg.HTTPMaxIdlePerHost,
		MaxConnsPerHost:     cfg.HTTPMaxConnsPerHost,
		IdleConnTimeout:     cfg.HTTPIdleConnTimeout,
	}

	client := &Client{
		httpClient: &http.Client{Timeout: cfg.Timeout, Transport: transport},
		transport:  transport,
		baseURL:    strings.TrimRight(baseURL, "/"),
		collection: cfg.Collection,
		apiKey:     cfg.APIKey,
		cfg:        cfg,
	}

	if err := client.ensureReady(ctx); err != nil {
		logger.Warn("vector: chromadb initialization failed", "collection", cfg.Collection, "error", err)
		return client, nil
	}
	logger.Info("vector: chromadb connection established")
	return client, nil
}

func (c *Client) Available() bool {
	return c != nil && c.available
}

func (c *Client) SetCollection(name string) {
	if c == nil {
		return
	}
	trimmed := strings.TrimSpace(name)
	if trimmed == "" {
		return
	}
	if trimmed == c.collection {
		return
	}
	c.mu.Lock()
	c.collection = trimmed
	c.collectionID = ""
	c.mu.Unlock()
}

func (c *Client) Collection() string {
	if c == nil {
		return ""
	}
	c.mu.RLock()
	defer c.mu.RUnlock()
	return c.collection
}

func (c *Client) ensureReady(ctx context.Context) error {
	if c == nil {
		return errors.New("chromadb client not configured")
	}
	c.mu.RLock()
	available := c.available
	collectionID := c.collectionID
	c.mu.RUnlock()

	if available && collectionID != "" {
		return nil
	}
	const maxAttempts = 3
	var err error
	for attempt := 0; attempt < maxAttempts; attempt++ {
		err = c.health(ctx)
		if err == nil {
			break
		}
		select {
		case <-ctx.Done():
			c.available = false
			return ctx.Err()
		case <-time.After(time.Duration(attempt+1) * 250 * time.Millisecond):
		}
	}
	if err != nil {
		c.available = false
		return err
	}
	if err = c.ensureCollectionID(ctx); err != nil {
		c.available = false
		return err
	}
	c.mu.Lock()
	c.available = true
	c.mu.Unlock()
	return nil
}

func (c *Client) EnsureCollection(ctx context.Context, dim int) error {
	if err := c.ensureReady(ctx); err != nil {
		return err
	}
	if !c.Available() {
		return errors.New("chromadb unavailable")
	}
	if dim <= 0 {
		return errors.New("invalid vector dimension")
	}
	return nil
}

func (c *Client) UpsertDocs(ctx context.Context, docs []kb.Doc, vectors [][]float32) error {
	if err := c.ensureReady(ctx); err != nil {
		return err
	}
	if !c.Available() {
		return errors.New("chromadb unavailable")
	}
	if len(docs) == 0 {
		return nil
	}
	ids := make([]string, 0, len(docs))
	embeddings := make([][]float32, 0, len(docs))
	documents := make([]string, 0, len(docs))
	metadatas := make([]map[string]interface{}, 0, len(docs))
	for idx, doc := range docs {
		ids = append(ids, doc.ID)
		if idx < len(vectors) {
			embeddings = append(embeddings, vectors[idx])
		} else {
			embeddings = append(embeddings, nil)
		}
		text := doc.Content
		if strings.TrimSpace(text) == "" {
			text = doc.Summary
		}
		documents = append(documents, text)
		metadatas = append(metadatas, metadataFromDoc(doc))
	}
	payload := map[string]interface{}{
		"ids":        ids,
		"documents":  documents,
		"metadatas":  metadatas,
		"embeddings": embeddings,
	}
	endpoint := fmt.Sprintf("%s/collections/%s/upsert", c.baseURL, url.PathEscape(c.collectionID))
	if err := c.doRequest(ctx, http.MethodPost, endpoint, payload, nil); err != nil {
		if errors.Is(err, errNotFound) {
			fallback := fmt.Sprintf("%s/collections/%s/add", c.baseURL, url.PathEscape(c.collectionID))
			if fallbackErr := c.doRequest(ctx, http.MethodPost, fallback, payload, nil); fallbackErr != nil {
				return fallbackErr
			}
			return nil
		}
		return err
	}
	return nil
}

func metadataFromDoc(doc kb.Doc) map[string]interface{} {
	metadata := make(map[string]interface{}, 4)
	metadata["program"] = doc.Program
	metadata["source"] = doc.SourcePath
	metadata["summary"] = doc.Summary
	if len(doc.Technologies) > 0 {
		joined := strings.Join(doc.Technologies, ", ")
		if strings.TrimSpace(joined) != "" {
			metadata["technologies"] = joined
		}
	}
	return metadata
}

func (c *Client) Search(ctx context.Context, vector []float32, limit int) ([]SearchResult, error) {
	if err := c.ensureReady(ctx); err != nil {
		return nil, err
	}
	if !c.Available() {
		return nil, errors.New("chromadb unavailable")
	}
	if limit <= 0 {
		limit = 5
	}
	body := map[string]interface{}{
		"query_embeddings": [][]float32{vector},
		"n_results":        limit,
	}
	endpoint := fmt.Sprintf("%s/collections/%s/query", c.baseURL, url.PathEscape(c.collectionID))
	var resp struct {
		IDs       [][]string                 `json:"ids"`
		Distances [][]float64                `json:"distances"`
		Metadatas [][]map[string]interface{} `json:"metadatas"`
		Documents [][]string                 `json:"documents"`
	}
	start := time.Now()
	if err := c.doRequest(ctx, http.MethodPost, endpoint, body, &resp); err != nil {
		telemetry.RecordVectorSearch(false, time.Since(start))
		return nil, err
	}
	telemetry.RecordVectorSearch(false, time.Since(start))
	if len(resp.IDs) == 0 {
		return nil, nil
	}
	results := make([]SearchResult, 0, len(resp.IDs[0]))
	for idx, id := range resp.IDs[0] {
		payload := map[string]interface{}{}
		if len(resp.Metadatas) > 0 && idx < len(resp.Metadatas[0]) {
			for k, v := range resp.Metadatas[0][idx] {
				payload[k] = v
			}
		}
		if len(resp.Documents) > 0 && idx < len(resp.Documents[0]) && resp.Documents[0][idx] != "" {
			payload["content"] = resp.Documents[0][idx]
		}
		score := float32(0)
		if len(resp.Distances) > 0 && idx < len(resp.Distances[0]) {
			dist := resp.Distances[0][idx]
			score = float32(1.0 / (1.0 + dist))
		}
		results = append(results, SearchResult{ID: id, Score: score, Payload: payload})
	}
	return results, nil
}

var _ Store = (*Client)(nil)

func (c *Client) ensureCollectionID(ctx context.Context) error {
	c.mu.RLock()
	if c.collectionID != "" {
		c.mu.RUnlock()
		return nil
	}
	c.mu.RUnlock()
	id, err := c.findCollection(ctx, c.collection)
	if err != nil {
		return err
	}
	if id != "" {
		c.mu.Lock()
		c.collectionID = id
		c.mu.Unlock()
		return nil
	}
	created, err := c.createCollection(ctx, c.collection)
	if err != nil {
		return err
	}
	c.mu.Lock()
	c.collectionID = created
	c.mu.Unlock()
	return nil
}

func (c *Client) findCollection(ctx context.Context, name string) (string, error) {
	endpoint := fmt.Sprintf("%s/collections?name=%s", c.baseURL, url.QueryEscape(name))
	var resp struct {
		Collections []struct {
			ID   string `json:"id"`
			Name string `json:"name"`
		} `json:"collections"`
	}
	if err := c.doRequest(ctx, http.MethodGet, endpoint, nil, &resp); err != nil {
		if errors.Is(err, errNotFound) {
			return "", nil
		}
		// Fallback to enumerating collections when the name filter is unsupported.
		endpoint = fmt.Sprintf("%s/collections", c.baseURL)
		if err := c.doRequest(ctx, http.MethodGet, endpoint, nil, &resp); err != nil {
			return "", err
		}
	}
	for _, col := range resp.Collections {
		if strings.EqualFold(col.Name, name) {
			return col.ID, nil
		}
	}
	return "", nil
}

func (c *Client) createCollection(ctx context.Context, name string) (string, error) {
	payload := map[string]interface{}{"name": name}
	endpoint := fmt.Sprintf("%s/collections", c.baseURL)
	var resp struct {
		ID string `json:"id"`
	}
	if err := c.doRequest(ctx, http.MethodPost, endpoint, payload, &resp); err != nil {
		if errors.Is(err, errConflict) {
			return c.findCollection(ctx, name)
		}
		return "", err
	}
	return resp.ID, nil
}

var (
	errNotFound = errors.New("resource not found")
	errConflict = errors.New("resource conflict")
)

func (c *Client) health(ctx context.Context) error {
	endpoint := fmt.Sprintf("%s/heartbeat", c.baseURL)
	return c.doRequest(ctx, http.MethodGet, endpoint, nil, nil)
}

func (c *Client) doRequest(ctx context.Context, method, endpoint string, body interface{}, out interface{}) error {
	if c == nil {
		return errors.New("chromadb client not configured")
	}
	var bodyReader io.Reader
	if body != nil {
		data, err := json.Marshal(body)
		if err != nil {
			return err
		}
		bodyReader = bytes.NewReader(data)
	}
	req, err := http.NewRequestWithContext(ctx, method, endpoint, bodyReader)
	if err != nil {
		return err
	}
	if body != nil {
		req.Header.Set("Content-Type", "application/json")
	}
	if c.apiKey != "" {
		req.Header.Set("Authorization", fmt.Sprintf("Bearer %s", c.apiKey))
	}
	resp, err := c.httpClient.Do(req)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	if resp.StatusCode == http.StatusNotFound {
		return errNotFound
	}
	if resp.StatusCode == http.StatusConflict {
		return errConflict
	}
	if resp.StatusCode < 200 || resp.StatusCode >= 300 {
		data, _ := io.ReadAll(resp.Body)
		return fmt.Errorf("chromadb %s %s failed: %s", method, endpoint, strings.TrimSpace(string(data)))
	}
	if out == nil {
		return nil
	}
	decoder := json.NewDecoder(resp.Body)
	return decoder.Decode(out)
}

// Close releases pooled resources associated with the client.
func (c *Client) Close() error {
	if c == nil {
		return nil
	}
	if c.transport != nil {
		c.transport.CloseIdleConnections()
	}
	return nil
}

func VectorDimension(v [][]float32) int {
	for _, vec := range v {
		if len(vec) > 0 {
			return len(vec)
		}
	}
	return 0
}
