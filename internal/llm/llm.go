// File path: internal/llm/llm.go
package llm

import (
	"errors"
	"os"
	"strings"
	"time"

	openai "github.com/openai/openai-go/v2"

	"github.com/nicodishanthj/Katral_phase1/internal/common"
	"github.com/nicodishanthj/Katral_phase1/internal/llm/providers"
)

type Message = providers.Message

type Provider = providers.Provider

func NewProvider() Provider {
	logger := common.Logger()
	apiKey := strings.TrimSpace(os.Getenv("OPENAI_API_KEY"))
	if apiKey != "" {
		opts := []openai.ClientOption{openai.WithAPIKey(apiKey)}
		if timeoutStr := strings.TrimSpace(os.Getenv("OPENAI_HTTP_TIMEOUT")); timeoutStr != "" {
			timeout, err := time.ParseDuration(timeoutStr)
			if err != nil {
				logger.Warn("llm: invalid OPENAI_HTTP_TIMEOUT, using default", "value", timeoutStr, "error", err)
			} else {
				logger.Info("llm: configuring OpenAI client with custom HTTP timeout", "timeout", timeout)
				opts = append(opts, openai.WithHTTPTimeout(timeout))
			}
		}
		if endpoint := strings.TrimSpace(os.Getenv("OPENAI_ENDPOINT")); endpoint != "" {
			logger.Info("llm: configuring OpenAI client with custom endpoint", "endpoint", endpoint)
			opts = append(opts, openai.WithBaseURL(endpoint))
		} else {
			logger.Debug("llm: using default OpenAI endpoint")
		}
		client := openai.NewClient(opts...)
		logger.Info("llm: OpenAI provider selected")
		return providers.NewOpenAIProvider(client)
	}
	logger.Warn("llm: OPENAI_API_KEY not set; falling back to local provider")
	return providers.NewLocalProvider()
}

func NormalizeMessages(messages []Message) ([]Message, error) {
	if len(messages) == 0 {
		return nil, errors.New("no messages provided")
	}
	for i := range messages {
		messages[i].Role = strings.ToLower(messages[i].Role)
	}
	return messages, nil
}
