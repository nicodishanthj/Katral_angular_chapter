// File path: internal/graph/kuzu/model.go
package kuzu

import "time"

// Statement represents a Cypher-like query with parameter bindings.
type Statement struct {
	Query  string
	Params map[string]interface{}
}

// ProgramNode captures the canonical properties for a COBOL program vertex.
type ProgramNode struct {
	ID           string
	Name         string
	SourcePath   string
	Summary      string
	Fingerprint  string
	Technologies []string
	Inputs       []string
	Outputs      []string
	UpdatedAt    time.Time
}

// FileNode models a data asset that can be consumed or produced by a program.
type FileNode struct {
	ID        string
	Name      string
	Kind      string
	Kinds     []string
	Path      string
	Summary   string
	UpdatedAt time.Time
}

// FlowNode captures a step within a program's flow diagram or narrative.
type FlowNode struct {
	ID        string
	ProgramID string
	Label     string
	Type      string
	Sequence  int
	Next      []string
	UpdatedAt time.Time
}

// BusinessRuleNode represents a derived business rule narrative for a program.
type BusinessRuleNode struct {
	ID        string
	ProgramID string
	Title     string
	Summary   string
	Content   string
	Tags      []string
	UpdatedAt time.Time
}

// CallEdge links two programs with a call relationship and associated weights.
type CallEdge struct {
	FromID      string
	ToID        string
	Weight      float64
	Occurrences int
	Notes       string
	UpdatedAt   time.Time
}

// InputEdge connects a program to an input file/data asset.
type InputEdge struct {
	ProgramID   string
	FileID      string
	Weight      float64
	Description string
	UpdatedAt   time.Time
}

// OutputEdge connects a program to an output file/data asset.
type OutputEdge struct {
	ProgramID   string
	FileID      string
	Weight      float64
	Description string
	UpdatedAt   time.Time
}

// FlowEdge associates a program with one of its flow steps.
type FlowEdge struct {
	ProgramID string
	StepID    string
	Weight    float64
	Label     string
	UpdatedAt time.Time
}

// BusinessRuleEdge joins a program to one of its business rule narratives.
type BusinessRuleEdge struct {
	ProgramID string
	RuleID    string
	Weight    float64
	Title     string
	UpdatedAt time.Time
}

// MutationBatch aggregates node and edge mutations for bulk ingestion.
type MutationBatch struct {
	Programs      []ProgramNode
	Files         []FileNode
	FlowSteps     []FlowNode
	BusinessRules []BusinessRuleNode
	Calls         []CallEdge
	Inputs        []InputEdge
	Outputs       []OutputEdge
	Flows         []FlowEdge
	RuleLinks     []BusinessRuleEdge
}

// Statements expands the mutation batch into ordered Cypher statements.
func (b MutationBatch) Statements() []Statement {
	var stmts []Statement
	if len(b.Programs) > 0 {
		stmts = append(stmts, Statement{
			Query: `UNWIND $programs AS program
MERGE (p:Program {id: program.id})
SET p.name = program.name,
    p.source = program.source_path,
    p.summary = program.summary,
    p.fingerprint = program.fingerprint,
    p.technologies = program.technologies,
    p.inputs = program.inputs,
    p.outputs = program.outputs,
    p.updated_at = datetime();`,
			Params: map[string]interface{}{"programs": toProgramParams(b.Programs)},
		})
	}
	if len(b.Files) > 0 {
		stmts = append(stmts, Statement{
			Query: `UNWIND $files AS file
MERGE (f:File {id: file.id})
SET f.name = file.name,
    f.kind = file.kind,
    f.kinds = file.kinds,
    f.path = file.path,
    f.summary = file.summary,
    f.updated_at = datetime();`,
			Params: map[string]interface{}{"files": toFileParams(b.Files)},
		})
	}
	if len(b.FlowSteps) > 0 {
		stmts = append(stmts, Statement{
			Query: `UNWIND $steps AS step
MERGE (fs:FlowStep {id: step.id})
SET fs.program_id = step.program_id,
    fs.label = step.label,
    fs.type = step.type,
    fs.sequence = step.sequence,
    fs.next_ids = step.next_ids,
    fs.updated_at = datetime();`,
			Params: map[string]interface{}{"steps": toFlowStepParams(b.FlowSteps)},
		})
	}
	if len(b.BusinessRules) > 0 {
		stmts = append(stmts, Statement{
			Query: `UNWIND $rules AS rule
MERGE (r:BusinessRule {id: rule.id})
SET r.program_id = rule.program_id,
    r.title = rule.title,
    r.summary = rule.summary,
    r.content = rule.content,
    r.tags = rule.tags,
    r.updated_at = datetime();`,
			Params: map[string]interface{}{"rules": toBusinessRuleParams(b.BusinessRules)},
		})
	}
	if len(b.Calls) > 0 {
		stmts = append(stmts, Statement{
			Query: `UNWIND $calls AS rel
MATCH (src:Program {id: rel.from_id})
MATCH (dst:Program {id: rel.to_id})
MERGE (src)-[edge:CALL]->(dst)
SET edge.weight = rel.weight,
    edge.occurrences = rel.occurrences,
    edge.notes = rel.notes,
    edge.updated_at = datetime();`,
			Params: map[string]interface{}{"calls": toCallParams(b.Calls)},
		})
	}
	if len(b.Inputs) > 0 {
		stmts = append(stmts, Statement{
			Query: `UNWIND $inputs AS rel
MATCH (src:Program {id: rel.program_id})
MATCH (dst:File {id: rel.file_id})
MERGE (src)-[edge:INPUT]->(dst)
SET edge.weight = rel.weight,
    edge.description = rel.description,
    edge.updated_at = datetime();`,
			Params: map[string]interface{}{"inputs": toInputParams(b.Inputs)},
		})
	}
	if len(b.Outputs) > 0 {
		stmts = append(stmts, Statement{
			Query: `UNWIND $outputs AS rel
MATCH (src:Program {id: rel.program_id})
MATCH (dst:File {id: rel.file_id})
MERGE (src)-[edge:OUTPUT]->(dst)
SET edge.weight = rel.weight,
    edge.description = rel.description,
    edge.updated_at = datetime();`,
			Params: map[string]interface{}{"outputs": toOutputParams(b.Outputs)},
		})
	}
	if len(b.Flows) > 0 {
		stmts = append(stmts, Statement{
			Query: `UNWIND $flows AS rel
MATCH (src:Program {id: rel.program_id})
MATCH (step:FlowStep {id: rel.step_id})
MERGE (src)-[edge:FLOW]->(step)
SET edge.weight = rel.weight,
    edge.label = rel.label,
    edge.updated_at = datetime();`,
			Params: map[string]interface{}{"flows": toFlowEdgeParams(b.Flows)},
		})
	}
	if len(b.RuleLinks) > 0 {
		stmts = append(stmts, Statement{
			Query: `UNWIND $rule_links AS rel
MATCH (src:Program {id: rel.program_id})
MATCH (rule:BusinessRule {id: rel.rule_id})
MERGE (src)-[edge:BUSINESS_RULES]->(rule)
SET edge.weight = rel.weight,
    edge.title = rel.title,
    edge.updated_at = datetime();`,
			Params: map[string]interface{}{"rule_links": toBusinessRuleEdgeParams(b.RuleLinks)},
		})
	}
	return stmts
}

func toProgramParams(programs []ProgramNode) []map[string]interface{} {
	result := make([]map[string]interface{}, 0, len(programs))
	for _, program := range programs {
		result = append(result, map[string]interface{}{
			"id":           program.ID,
			"name":         program.Name,
			"source_path":  program.SourcePath,
			"summary":      program.Summary,
			"fingerprint":  program.Fingerprint,
			"technologies": program.Technologies,
			"inputs":       program.Inputs,
			"outputs":      program.Outputs,
			"updated_at":   program.UpdatedAt,
		})
	}
	return result
}

func toFileParams(files []FileNode) []map[string]interface{} {
	result := make([]map[string]interface{}, 0, len(files))
	for _, file := range files {
		result = append(result, map[string]interface{}{
			"id":         file.ID,
			"name":       file.Name,
			"kind":       file.Kind,
			"kinds":      file.Kinds,
			"path":       file.Path,
			"summary":    file.Summary,
			"updated_at": file.UpdatedAt,
		})
	}
	return result
}

func toFlowStepParams(steps []FlowNode) []map[string]interface{} {
	result := make([]map[string]interface{}, 0, len(steps))
	for _, step := range steps {
		result = append(result, map[string]interface{}{
			"id":         step.ID,
			"program_id": step.ProgramID,
			"label":      step.Label,
			"type":       step.Type,
			"sequence":   step.Sequence,
			"next_ids":   step.Next,
			"updated_at": step.UpdatedAt,
		})
	}
	return result
}

func toBusinessRuleParams(rules []BusinessRuleNode) []map[string]interface{} {
	result := make([]map[string]interface{}, 0, len(rules))
	for _, rule := range rules {
		result = append(result, map[string]interface{}{
			"id":         rule.ID,
			"program_id": rule.ProgramID,
			"title":      rule.Title,
			"summary":    rule.Summary,
			"content":    rule.Content,
			"tags":       rule.Tags,
			"updated_at": rule.UpdatedAt,
		})
	}
	return result
}

func toCallParams(calls []CallEdge) []map[string]interface{} {
	result := make([]map[string]interface{}, 0, len(calls))
	for _, call := range calls {
		result = append(result, map[string]interface{}{
			"from_id":     call.FromID,
			"to_id":       call.ToID,
			"weight":      call.Weight,
			"occurrences": call.Occurrences,
			"notes":       call.Notes,
			"updated_at":  call.UpdatedAt,
		})
	}
	return result
}

func toInputParams(inputs []InputEdge) []map[string]interface{} {
	result := make([]map[string]interface{}, 0, len(inputs))
	for _, input := range inputs {
		result = append(result, map[string]interface{}{
			"program_id":  input.ProgramID,
			"file_id":     input.FileID,
			"weight":      input.Weight,
			"description": input.Description,
			"updated_at":  input.UpdatedAt,
		})
	}
	return result
}

func toOutputParams(outputs []OutputEdge) []map[string]interface{} {
	result := make([]map[string]interface{}, 0, len(outputs))
	for _, output := range outputs {
		result = append(result, map[string]interface{}{
			"program_id":  output.ProgramID,
			"file_id":     output.FileID,
			"weight":      output.Weight,
			"description": output.Description,
			"updated_at":  output.UpdatedAt,
		})
	}
	return result
}

func toFlowEdgeParams(flows []FlowEdge) []map[string]interface{} {
	result := make([]map[string]interface{}, 0, len(flows))
	for _, flow := range flows {
		result = append(result, map[string]interface{}{
			"program_id": flow.ProgramID,
			"step_id":    flow.StepID,
			"weight":     flow.Weight,
			"label":      flow.Label,
			"updated_at": flow.UpdatedAt,
		})
	}
	return result
}

func toBusinessRuleEdgeParams(edges []BusinessRuleEdge) []map[string]interface{} {
	result := make([]map[string]interface{}, 0, len(edges))
	for _, edge := range edges {
		result = append(result, map[string]interface{}{
			"program_id": edge.ProgramID,
			"rule_id":    edge.RuleID,
			"weight":     edge.Weight,
			"title":      edge.Title,
			"updated_at": edge.UpdatedAt,
		})
	}
	return result
}
