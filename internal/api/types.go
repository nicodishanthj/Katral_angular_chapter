// File path: internal/api/types.go
package api

import (
	"github.com/nicodishanthj/Katral_phase1/internal/agent"
	"github.com/nicodishanthj/Katral_phase1/internal/workflow"
)

type ingestRequest = workflow.IngestRequest

type ingestResponse = workflow.IngestResponse

type ingestUploadResponse struct {
	Uploaded   int    `json:"uploaded"`
	Repo       string `json:"repo,omitempty"`
	Workspace  string `json:"workspace,omitempty"`
	Documents  int    `json:"documents"`
	Collection string `json:"collection,omitempty"`
	Warning    string `json:"warning,omitempty"`
}

type chatRequest struct {
	Prompt     string `json:"prompt"`
	UseRAG     bool   `json:"use_rag"`
	Collection string `json:"collection"`
}

type agentRequest struct {
	Goal            string                 `json:"goal"`
	Flow            string                 `json:"flow"`
	KnowledgeConfig *agent.KnowledgeConfig `json:"knowledge_config"`
	TargetConfig    *agent.TargetConfig    `json:"target_config"`
}
