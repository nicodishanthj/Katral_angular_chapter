// File path: internal/graph/kuzu/client_test.go
package kuzu

import (
	"context"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"testing"
	"time"
)

type queryPayload struct {
	Query string `json:"query"`
}

func TestNewClientPingRespectsConfiguredTimeout(t *testing.T) {
	tests := []struct {
		name          string
		requestDelay  time.Duration
		clientTimeout time.Duration
		wantAvailable bool
	}{
		{
			name:          "within-timeout",
			requestDelay:  400 * time.Millisecond,
			clientTimeout: 2 * time.Second,
			wantAvailable: true,
		},
		{
			name:          "exceeds-timeout",
			requestDelay:  1500 * time.Millisecond,
			clientTimeout: 1 * time.Second,
			wantAvailable: false,
		},
	}

	for _, tc := range tests {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				if r.URL.Path != "/query" {
					http.NotFound(w, r)
					return
				}
				decoder := json.NewDecoder(r.Body)
				var payload queryPayload
				if err := decoder.Decode(&payload); err != nil {
					t.Fatalf("decode payload: %v", err)
				}
				if payload.Query == "" {
					t.Fatalf("empty query payload")
				}
				time.Sleep(tc.requestDelay)
				w.Header().Set("Content-Type", "application/json")
				if _, err := w.Write([]byte(`{"error":""}`)); err != nil {
					t.Fatalf("write response: %v", err)
				}
			}))
			defer server.Close()

			cfg := Config{
				Endpoint:       server.URL,
				Database:       "main",
				MaxConnections: 1,
				Timeout:        tc.clientTimeout,
			}

			client, err := NewClient(context.Background(), cfg)
			if err != nil {
				t.Fatalf("NewClient() error = %v", err)
			}
			t.Cleanup(func() {
				_ = client.Close()
			})

			if got := client.Available(); got != tc.wantAvailable {
				t.Fatalf("client.Available() = %v, want %v", got, tc.wantAvailable)
			}
		})
	}
}
