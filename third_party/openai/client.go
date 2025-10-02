// File path: third_party/openai/client.go
package openai

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	neturl "net/url"
	"strings"
	"time"
)

type Client struct {
	apiKey     string
	baseURL    string
	httpClient *http.Client
}

type ClientOption func(*Client)

func WithAPIKey(key string) ClientOption {
	return func(c *Client) {
		c.apiKey = key
	}
}

func WithHTTPTimeout(timeout time.Duration) ClientOption {
	return func(c *Client) {
		if c.httpClient == nil {
			c.httpClient = &http.Client{}
		}
		c.httpClient.Timeout = timeout
	}
}

func WithBaseURL(rawURL string) ClientOption {
	return func(c *Client) {
		c.baseURL = normalizeBaseURL(rawURL)
	}
}

func normalizeBaseURL(raw string) string {
	trimmed := strings.TrimSpace(raw)
	if trimmed == "" {
		return ""
	}

	trimmed = strings.TrimRight(trimmed, "/")
	if trimmed == "" {
		return "/v1"
	}

	parsed, err := neturl.Parse(trimmed)
	if err != nil || parsed.Scheme == "" || parsed.Host == "" {
		if strings.HasSuffix(trimmed, "/v1") {
			return trimmed
		}
		return trimmed + "/v1"
	}

	path := strings.Trim(parsed.Path, "/")
	if path == "" {
		parsed.Path = "/v1"
	} else {
		segments := strings.Split(path, "/")
		hasV1 := false
		for _, segment := range segments {
			if segment == "v1" {
				hasV1 = true
				break
			}
		}
		if !hasV1 {
			segments = append(segments, "v1")
		}
		parsed.Path = "/" + strings.Join(segments, "/")
	}

	parsed.RawPath = ""

	return parsed.String()
}

func NewClient(opts ...ClientOption) *Client {
	c := &Client{
		baseURL:    normalizeBaseURL("https://api.openai.com/v1"),
		httpClient: &http.Client{Timeout: 30 * time.Second},
	}
	for _, opt := range opts {
		opt(c)
	}
	return c
}

var (
	Chat       = &ChatCompletionsService{}
	Embeddings = &EmbeddingsService{}
)

type ChatCompletionsService struct {
	client *Client
}

type EmbeddingsService struct {
	client *Client
}

type ChatCompletionMessageParam struct {
	Role    string `json:"role"`
	Content string `json:"content"`
}

type ChatCompletionCreateParams struct {
	Model    string                       `json:"model"`
	Messages []ChatCompletionMessageParam `json:"messages"`
}

type ChatCompletion struct {
	Choices []ChatCompletionChoice `json:"choices"`
}

type ChatCompletionChoice struct {
	Message ChatMessage `json:"message"`
}

type ChatMessage struct {
	Role    string `json:"role"`
	Content string `json:"content"`
}

type EmbeddingCreateParams struct {
	Model string   `json:"model"`
	Input []string `json:"input"`
}

type Embedding struct {
	Data []EmbeddingData `json:"data"`
}

type EmbeddingData struct {
	Embedding []float32 `json:"embedding"`
}

func (s *ChatCompletionsService) Create(ctx context.Context, c *Client, params ChatCompletionCreateParams) (ChatCompletion, error) {
	if c == nil {
		return ChatCompletion{}, fmt.Errorf("nil client")
	}
	payload, err := json.Marshal(params)
	if err != nil {
		return ChatCompletion{}, err
	}
	endpoint, err := neturl.JoinPath(c.baseURL, "chat", "completions")
	if err != nil {
		return ChatCompletion{}, err
	}
	req, err := http.NewRequestWithContext(ctx, http.MethodPost, endpoint, bytes.NewReader(payload))
	if err != nil {
		return ChatCompletion{}, err
	}
	req.Header.Set("Content-Type", "application/json")
	if c.apiKey != "" {
		req.Header.Set("Authorization", "Bearer "+c.apiKey)
	}
	resp, err := c.httpClient.Do(req)
	if err != nil {
		return ChatCompletion{}, err
	}
	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return ChatCompletion{}, err
	}
	if resp.StatusCode >= 400 {
		return ChatCompletion{}, apiError("chat", resp.Status, body)
	}
	var out ChatCompletion
	if err := json.Unmarshal(body, &out); err != nil {
		return ChatCompletion{}, err
	}
	return out, nil
}

func (s *EmbeddingsService) Create(ctx context.Context, c *Client, params EmbeddingCreateParams) (Embedding, error) {
	if c == nil {
		return Embedding{}, fmt.Errorf("nil client")
	}
	payload, err := json.Marshal(params)
	if err != nil {
		return Embedding{}, err
	}
	endpoint, err := neturl.JoinPath(c.baseURL, "embeddings")
	if err != nil {
		return Embedding{}, err
	}
	req, err := http.NewRequestWithContext(ctx, http.MethodPost, endpoint, bytes.NewReader(payload))
	if err != nil {
		return Embedding{}, err
	}
	req.Header.Set("Content-Type", "application/json")
	if c.apiKey != "" {
		req.Header.Set("Authorization", "Bearer "+c.apiKey)
	}
	resp, err := c.httpClient.Do(req)
	if err != nil {
		return Embedding{}, err
	}
	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return Embedding{}, err
	}
	if resp.StatusCode >= 400 {
		return Embedding{}, apiError("embedding", resp.Status, body)
	}
	var out Embedding
	if err := json.Unmarshal(body, &out); err != nil {
		return Embedding{}, err
	}
	return out, nil
}

func (c *Client) With() *Client {
	return c
}

func (c *Client) ChatService() *ChatCompletionsService {
	svc := *Chat
	svc.client = c
	return &svc
}

func (c *Client) EmbeddingsService() *EmbeddingsService {
	svc := *Embeddings
	svc.client = c
	return &svc
}

type apiErrorResponse struct {
	Error struct {
		Message string      `json:"message"`
		Type    string      `json:"type"`
		Param   string      `json:"param"`
		Code    interface{} `json:"code"`
	} `json:"error"`
}

func apiError(kind, status string, body []byte) error {
	if len(body) == 0 {
		return fmt.Errorf("openai %s error: %s", kind, status)
	}
	var apiErr apiErrorResponse
	if err := json.Unmarshal(body, &apiErr); err == nil && apiErr.Error.Message != "" {
		message := apiErr.Error.Message
		if apiErr.Error.Code != nil && apiErr.Error.Code != "" {
			return fmt.Errorf("openai %s error: %s (code=%v)", kind, message, apiErr.Error.Code)
		}
		return fmt.Errorf("openai %s error: %s", kind, message)
	}
	trimmed := strings.TrimSpace(string(body))
	if trimmed == "" {
		return fmt.Errorf("openai %s error: %s", kind, status)
	}
	return fmt.Errorf("openai %s error: %s: %s", kind, status, trimmed)
}
