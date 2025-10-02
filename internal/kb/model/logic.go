// File path: internal/kb/model/logic.go
package model

// LogicStep represents a normalized unit of COBOL procedure logic.
type LogicStep struct {
	Paragraph string            `json:"paragraph,omitempty"`
	Type      string            `json:"type"`
	Condition string            `json:"condition,omitempty"`
	Action    string            `json:"action,omitempty"`
	Targets   []string          `json:"targets,omitempty"`
	Data      map[string]string `json:"data,omitempty"`
	Children  []*LogicStep      `json:"children,omitempty"`
	Else      []*LogicStep      `json:"else,omitempty"`
	Raw       string            `json:"raw,omitempty"`
}
