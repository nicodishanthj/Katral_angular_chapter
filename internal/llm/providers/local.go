// File path: internal/llm/providers/local.go
package providers

import (
	"context"
	"fmt"
	"strings"
)

type Message struct {
	Role    string
	Content string
}

type Provider interface {
	Chat(ctx context.Context, messages []Message) (string, error)
	Embed(ctx context.Context, input []string) ([][]float32, error)
	Name() string
}

type LocalProvider struct{}

func NewLocalProvider() *LocalProvider {
	return &LocalProvider{}
}

func (l *LocalProvider) Chat(ctx context.Context, messages []Message) (string, error) {
	if len(messages) == 0 {
		return "", fmt.Errorf("no messages provided")
	}
	last := messages[len(messages)-1].Content
	return "[local-stub] " + strings.TrimSpace(last), nil
}

func (l *LocalProvider) Embed(ctx context.Context, input []string) ([][]float32, error) {
	vectors := make([][]float32, len(input))
	for i := range input {
		vectors[i] = []float32{0, 0, 0}
	}
	return vectors, nil
}

func (l *LocalProvider) Name() string {
	return "local"
}
