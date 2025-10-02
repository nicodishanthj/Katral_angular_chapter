// File path: internal/llm/providers/openai_client.go
package providers

import (
	"context"
	"fmt"
	"os"

	openai "github.com/openai/openai-go/v2"

	"github.com/nicodishanthj/Katral_phase1/internal/common"
)

type OpenAIProvider struct {
	client     *openai.Client
	chatModel  string
	embedModel string
}

func NewOpenAIProvider(client *openai.Client) *OpenAIProvider {
	chatModel := os.Getenv("OPENAI_CHAT_MODEL")
	if chatModel == "" {
		chatModel = "gpt-4o"
	}
	embedModel := os.Getenv("OPENAI_EMBED_MODEL")
	if embedModel == "" {
		embedModel = "text-embedding-3-small"
	}
	logger := common.Logger()
	logger.Info("llm: OpenAI provider configured", "chat_model", chatModel, "embed_model", embedModel)
	return &OpenAIProvider{client: client, chatModel: chatModel, embedModel: embedModel}
}

func (o *OpenAIProvider) Chat(ctx context.Context, messages []Message) (string, error) {
	if o.client == nil {
		return "", fmt.Errorf("nil openai client")
	}
	logger := common.Logger()
	logger.Debug("llm: sending chat completion request", "model", o.chatModel, "messages", len(messages))
	params := openai.ChatCompletionCreateParams{Model: o.chatModel}
	for _, msg := range messages {
		params.Messages = append(params.Messages, openai.ChatCompletionMessageParam{Role: msg.Role, Content: msg.Content})
	}
	resp, err := openai.Chat.Create(ctx, o.client, params)
	if err != nil {
		logger.Error("llm: chat completion failed", "error", err)
		return "", err
	}
	if len(resp.Choices) == 0 {
		return "", fmt.Errorf("no choices returned")
	}
	logger.Debug("llm: chat completion succeeded")
	return resp.Choices[0].Message.Content, nil
}

func (o *OpenAIProvider) Embed(ctx context.Context, input []string) ([][]float32, error) {
	if o.client == nil {
		return nil, fmt.Errorf("nil openai client")
	}
	if len(input) == 0 {
		return nil, nil
	}
	logger := common.Logger()
	logger.Debug("llm: creating embeddings", "model", o.embedModel, "items", len(input))
	resp, err := openai.Embeddings.Create(ctx, o.client, openai.EmbeddingCreateParams{Model: o.embedModel, Input: input})
	if err != nil {
		logger.Error("llm: embedding request failed", "error", err)
		return nil, err
	}
	vectors := make([][]float32, 0, len(resp.Data))
	for _, data := range resp.Data {
		vectors = append(vectors, data.Embedding)
	}
	logger.Debug("llm: embedding request succeeded", "returned", len(vectors))
	return vectors, nil
}

func (o *OpenAIProvider) Name() string {
	return "openai"
}
