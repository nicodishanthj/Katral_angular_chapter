// File path: third_party/openai/client_test.go
package openai

import (
	"context"
	"net/http"
	"net/http/httptest"
	"testing"
	"time"
)

func TestWithHTTPTimeout(t *testing.T) {
	t.Parallel()

	cases := []struct {
		name    string
		timeout time.Duration
	}{
		{name: "ShortTimeout", timeout: 5 * time.Second},
		{name: "ZeroTimeout", timeout: 0},
		{name: "LongTimeout", timeout: 2 * time.Minute},
	}

	for _, tc := range cases {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			client := NewClient(WithHTTPTimeout(tc.timeout))
			if client.httpClient == nil {
				t.Fatalf("http client is nil")
			}
			if client.httpClient.Timeout != tc.timeout {
				t.Fatalf("timeout = %s, want %s", client.httpClient.Timeout, tc.timeout)
			}
		})
	}
}

func TestNormalizeBaseURL(t *testing.T) {
	t.Parallel()

	cases := []struct {
		name string
		in   string
		want string
	}{
		{
			name: "OpenAIWithoutV1",
			in:   "https://api.openai.com",
			want: "https://api.openai.com/v1",
		},
		{
			name: "OpenAIWithV1",
			in:   "https://api.openai.com/v1",
			want: "https://api.openai.com/v1",
		},
		{
			name: "LiteLLMWithoutV1",
			in:   "https://litellm.example.com",
			want: "https://litellm.example.com/v1",
		},
		{
			name: "LiteLLMWithV1",
			in:   "https://litellm.example.com/v1",
			want: "https://litellm.example.com/v1",
		},
		{
			name: "TrailingSlash",
			in:   "https://api.openai.com/v1/",
			want: "https://api.openai.com/v1",
		},
	}

	for _, tc := range cases {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got := normalizeBaseURL(tc.in)
			if got != tc.want {
				t.Fatalf("normalizeBaseURL(%q) = %q, want %q", tc.in, got, tc.want)
			}
		})
	}
}

func TestClientRequestsUseNormalizedBase(t *testing.T) {
	t.Parallel()

	cases := []struct {
		name              string
		baseSuffix        string
		wantChatPath      string
		wantEmbeddingPath string
	}{
		{
			name:              "NativeWithoutV1",
			baseSuffix:        "",
			wantChatPath:      "/v1/chat/completions",
			wantEmbeddingPath: "/v1/embeddings",
		},
		{
			name:              "NativeWithV1",
			baseSuffix:        "/v1",
			wantChatPath:      "/v1/chat/completions",
			wantEmbeddingPath: "/v1/embeddings",
		},
		{
			name:              "LiteLLMWithoutV1",
			baseSuffix:        "/router",
			wantChatPath:      "/router/v1/chat/completions",
			wantEmbeddingPath: "/router/v1/embeddings",
		},
		{
			name:              "LiteLLMWithV1",
			baseSuffix:        "/router/v1",
			wantChatPath:      "/router/v1/chat/completions",
			wantEmbeddingPath: "/router/v1/embeddings",
		},
		{
			name:              "LiteLLMWithTrailingSlash",
			baseSuffix:        "/router/v1/",
			wantChatPath:      "/router/v1/chat/completions",
			wantEmbeddingPath: "/router/v1/embeddings",
		},
	}

	for _, tc := range cases {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			chatCalled := make(chan struct{}, 1)
			embeddingCalled := make(chan struct{}, 1)

			server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				switch r.URL.Path {
				case tc.wantChatPath:
					select {
					case chatCalled <- struct{}{}:
					default:
					}
					w.Header().Set("Content-Type", "application/json")
					_, _ = w.Write([]byte(`{"choices":[{"message":{"role":"assistant","content":"ok"}}]}`))
				case tc.wantEmbeddingPath:
					select {
					case embeddingCalled <- struct{}{}:
					default:
					}
					w.Header().Set("Content-Type", "application/json")
					_, _ = w.Write([]byte(`{"data":[{"embedding":[0.1]}]}`))
				default:
					t.Errorf("unexpected path %q", r.URL.Path)
					http.NotFound(w, r)
				}
			}))
			defer server.Close()

			base := server.URL + tc.baseSuffix
			client := NewClient(WithBaseURL(base))
			client.httpClient = server.Client()

			expectedBase := normalizeBaseURL(base)
			if client.baseURL != expectedBase {
				t.Fatalf("normalized base url = %q, want %q", client.baseURL, expectedBase)
			}

			_, err := client.ChatService().Create(context.Background(), client, ChatCompletionCreateParams{
				Model: "gpt",
				Messages: []ChatCompletionMessageParam{{
					Role:    "user",
					Content: "Hello",
				}},
			})
			if err != nil {
				t.Fatalf("chat create error: %v", err)
			}

			_, err = client.EmbeddingsService().Create(context.Background(), client, EmbeddingCreateParams{
				Model: "embed",
				Input: []string{"hi"},
			})
			if err != nil {
				t.Fatalf("embedding create error: %v", err)
			}

			select {
			case <-chatCalled:
			default:
				t.Fatalf("chat endpoint not invoked for base %q", base)
			}

			select {
			case <-embeddingCalled:
			default:
				t.Fatalf("embeddings endpoint not invoked for base %q", base)
			}
		})
	}
}
