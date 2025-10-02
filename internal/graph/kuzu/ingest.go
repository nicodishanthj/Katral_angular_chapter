// File path: internal/graph/kuzu/ingest.go
package kuzu

import (
	"fmt"
	"sort"
	"strings"
	"time"
	"unicode"

	"github.com/nicodishanthj/Katral_phase1/internal/kb"
)

// BatchBuilder incrementally aggregates knowledge base documents into a mutation batch.
type BatchBuilder struct {
	programs      map[string]*ProgramNode
	files         map[string]*FileNode
	flowSteps     map[string]*FlowNode
	businessRules map[string]*BusinessRuleNode
	calls         map[string]*CallEdge
	inputs        map[string]*InputEdge
	outputs       map[string]*OutputEdge
	flows         map[string]*FlowEdge
	ruleLinks     map[string]*BusinessRuleEdge
}

// NewBatchBuilder constructs an empty builder instance.
func NewBatchBuilder() *BatchBuilder {
	return &BatchBuilder{
		programs:      make(map[string]*ProgramNode),
		files:         make(map[string]*FileNode),
		flowSteps:     make(map[string]*FlowNode),
		businessRules: make(map[string]*BusinessRuleNode),
		calls:         make(map[string]*CallEdge),
		inputs:        make(map[string]*InputEdge),
		outputs:       make(map[string]*OutputEdge),
		flows:         make(map[string]*FlowEdge),
		ruleLinks:     make(map[string]*BusinessRuleEdge),
	}
}

// AddDoc folds a knowledge base document into the accumulated mutation state.
func (b *BatchBuilder) AddDoc(doc kb.Doc) {
	programID := canonicalProgramID(doc.Program)
	if programID == "" {
		return
	}
	timestamp := time.Now().UTC()

	program := b.ensureProgram(programID)
	program.UpdatedAt = timestamp
	if program.Name == "" {
		program.Name = strings.TrimSpace(doc.Program)
	}
	if strings.TrimSpace(doc.SourcePath) != "" {
		program.SourcePath = strings.TrimSpace(doc.SourcePath)
	}
	if strings.TrimSpace(doc.Summary) != "" && strings.TrimSpace(program.Summary) == "" {
		program.Summary = strings.TrimSpace(doc.Summary)
	}
	if strings.TrimSpace(doc.Fingerprint) != "" {
		program.Fingerprint = strings.TrimSpace(doc.Fingerprint)
	}
	program.Technologies = appendUnique(program.Technologies, doc.Technologies...)
	program.Inputs = appendUnique(program.Inputs, doc.Inputs...)
	program.Outputs = appendUnique(program.Outputs, doc.Outputs...)

	for _, raw := range doc.Calls {
		calleeID := canonicalProgramID(raw)
		if calleeID == "" {
			continue
		}
		key := programID + "|" + calleeID
		call := b.ensureCallEdge(key, programID, calleeID)
		call.Occurrences++
		call.Weight = float64(call.Occurrences)
		call.UpdatedAt = timestamp
	}

	for _, input := range doc.Inputs {
		fileID := canonicalFileID(input)
		if fileID == "" {
			continue
		}
		file := b.ensureFile(fileID, strings.TrimSpace(input))
		file.UpdatedAt = timestamp
		file.Kinds = appendUnique(file.Kinds, "input")
		if file.Kind == "" {
			file.Kind = "input"
		}
		key := programID + "|IN|" + fileID
		edge := b.ensureInputEdge(key, programID, fileID)
		edge.Weight++
		edge.Description = strings.TrimSpace(input)
		edge.UpdatedAt = timestamp
	}

	for _, output := range doc.Outputs {
		fileID := canonicalFileID(output)
		if fileID == "" {
			continue
		}
		file := b.ensureFile(fileID, strings.TrimSpace(output))
		file.UpdatedAt = timestamp
		file.Kinds = appendUnique(file.Kinds, "output")
		if file.Kind == "" {
			file.Kind = "output"
		}
		key := programID + "|OUT|" + fileID
		edge := b.ensureOutputEdge(key, programID, fileID)
		edge.Weight++
		edge.Description = strings.TrimSpace(output)
		edge.UpdatedAt = timestamp
	}

	if doc.FlowDiagram != nil {
		for idx, step := range doc.FlowDiagram.Nodes {
			stepID := flowNodeID(doc.ID, step.ID)
			flow := b.ensureFlowNode(stepID)
			flow.ProgramID = programID
			flow.UpdatedAt = timestamp
			if strings.TrimSpace(flow.Label) == "" {
				flow.Label = strings.TrimSpace(step.Label)
			}
			if strings.TrimSpace(flow.Type) == "" {
				flow.Type = strings.TrimSpace(step.Type)
			}
			if flow.Sequence == 0 {
				flow.Sequence = idx + 1
			}
			key := programID + "|FLOW|" + stepID
			edge := b.ensureFlowEdge(key, programID, stepID)
			edge.Label = flow.Label
			if edge.Weight == 0 {
				edge.Weight = 1
			}
			edge.UpdatedAt = timestamp
		}
		for _, rel := range doc.FlowDiagram.Edges {
			from := flowNodeID(doc.ID, rel.From)
			to := flowNodeID(doc.ID, rel.To)
			node := b.ensureFlowNode(from)
			node.ProgramID = programID
			node.UpdatedAt = timestamp
			label := strings.TrimSpace(rel.Label)
			if label != "" {
				// Carry the labeled transition on the owning FLOW edge if available.
				edgeKey := programID + "|FLOW|" + from
				edge := b.ensureFlowEdge(edgeKey, programID, from)
				if strings.TrimSpace(edge.Label) == "" {
					edge.Label = label
				}
			}
			node.Next = appendUnique(node.Next, to)
		}
	}

	if strings.EqualFold(strings.TrimSpace(doc.Type), "business_rules") {
		ruleID := doc.ID
		if strings.TrimSpace(ruleID) == "" {
			ruleID = programID + ":rule:" + slugify(firstNonEmpty(doc.Summary, doc.Content))
		}
		rule := b.ensureBusinessRule(ruleID)
		rule.ProgramID = programID
		rule.UpdatedAt = timestamp
		title := firstNonEmpty(doc.Summary, firstLine(doc.Content))
		if strings.TrimSpace(rule.Title) == "" {
			rule.Title = strings.TrimSpace(title)
		}
		if strings.TrimSpace(doc.Summary) != "" {
			rule.Summary = strings.TrimSpace(doc.Summary)
		}
		if strings.TrimSpace(doc.Content) != "" {
			rule.Content = strings.TrimSpace(doc.Content)
		}
		rule.Tags = appendUnique(rule.Tags, doc.Technologies...)

		edgeKey := programID + "|RULE|" + ruleID
		link := b.ensureRuleLink(edgeKey, programID, ruleID)
		if strings.TrimSpace(link.Title) == "" {
			link.Title = rule.Title
		}
		if link.Weight == 0 {
			link.Weight = 1
		}
		link.UpdatedAt = timestamp
	}
}

// Batch finalizes the accumulated state into a MutationBatch.
func (b *BatchBuilder) Batch() MutationBatch {
	batch := MutationBatch{}
	if len(b.programs) > 0 {
		batch.Programs = make([]ProgramNode, 0, len(b.programs))
		for _, node := range b.programs {
			batch.Programs = append(batch.Programs, *node)
		}
		sort.Slice(batch.Programs, func(i, j int) bool { return batch.Programs[i].ID < batch.Programs[j].ID })
	}
	if len(b.files) > 0 {
		batch.Files = make([]FileNode, 0, len(b.files))
		for _, node := range b.files {
			// Keep kinds sorted for deterministic output.
			sort.Strings(node.Kinds)
			batch.Files = append(batch.Files, *node)
		}
		sort.Slice(batch.Files, func(i, j int) bool { return batch.Files[i].ID < batch.Files[j].ID })
	}
	if len(b.flowSteps) > 0 {
		batch.FlowSteps = make([]FlowNode, 0, len(b.flowSteps))
		for _, node := range b.flowSteps {
			sort.Strings(node.Next)
			batch.FlowSteps = append(batch.FlowSteps, *node)
		}
		sort.Slice(batch.FlowSteps, func(i, j int) bool { return batch.FlowSteps[i].ID < batch.FlowSteps[j].ID })
	}
	if len(b.businessRules) > 0 {
		batch.BusinessRules = make([]BusinessRuleNode, 0, len(b.businessRules))
		for _, node := range b.businessRules {
			sort.Strings(node.Tags)
			batch.BusinessRules = append(batch.BusinessRules, *node)
		}
		sort.Slice(batch.BusinessRules, func(i, j int) bool { return batch.BusinessRules[i].ID < batch.BusinessRules[j].ID })
	}
	if len(b.calls) > 0 {
		batch.Calls = make([]CallEdge, 0, len(b.calls))
		for _, edge := range b.calls {
			batch.Calls = append(batch.Calls, *edge)
		}
		sort.Slice(batch.Calls, func(i, j int) bool {
			if batch.Calls[i].FromID == batch.Calls[j].FromID {
				return batch.Calls[i].ToID < batch.Calls[j].ToID
			}
			return batch.Calls[i].FromID < batch.Calls[j].FromID
		})
	}
	if len(b.inputs) > 0 {
		batch.Inputs = make([]InputEdge, 0, len(b.inputs))
		for _, edge := range b.inputs {
			batch.Inputs = append(batch.Inputs, *edge)
		}
		sort.Slice(batch.Inputs, func(i, j int) bool {
			if batch.Inputs[i].ProgramID == batch.Inputs[j].ProgramID {
				return batch.Inputs[i].FileID < batch.Inputs[j].FileID
			}
			return batch.Inputs[i].ProgramID < batch.Inputs[j].ProgramID
		})
	}
	if len(b.outputs) > 0 {
		batch.Outputs = make([]OutputEdge, 0, len(b.outputs))
		for _, edge := range b.outputs {
			batch.Outputs = append(batch.Outputs, *edge)
		}
		sort.Slice(batch.Outputs, func(i, j int) bool {
			if batch.Outputs[i].ProgramID == batch.Outputs[j].ProgramID {
				return batch.Outputs[i].FileID < batch.Outputs[j].FileID
			}
			return batch.Outputs[i].ProgramID < batch.Outputs[j].ProgramID
		})
	}
	if len(b.flows) > 0 {
		batch.Flows = make([]FlowEdge, 0, len(b.flows))
		for _, edge := range b.flows {
			batch.Flows = append(batch.Flows, *edge)
		}
		sort.Slice(batch.Flows, func(i, j int) bool {
			if batch.Flows[i].ProgramID == batch.Flows[j].ProgramID {
				return batch.Flows[i].StepID < batch.Flows[j].StepID
			}
			return batch.Flows[i].ProgramID < batch.Flows[j].ProgramID
		})
	}
	if len(b.ruleLinks) > 0 {
		batch.RuleLinks = make([]BusinessRuleEdge, 0, len(b.ruleLinks))
		for _, edge := range b.ruleLinks {
			batch.RuleLinks = append(batch.RuleLinks, *edge)
		}
		sort.Slice(batch.RuleLinks, func(i, j int) bool {
			if batch.RuleLinks[i].ProgramID == batch.RuleLinks[j].ProgramID {
				return batch.RuleLinks[i].RuleID < batch.RuleLinks[j].RuleID
			}
			return batch.RuleLinks[i].ProgramID < batch.RuleLinks[j].ProgramID
		})
	}
	return batch
}

// DependencyChainQuery builds a traversal query for downstream dependencies via CALL or FLOW edges.
func DependencyChainQuery(programID string, depth int) Statement {
	if depth <= 0 {
		depth = 5
	}
	query := fmt.Sprintf(`MATCH path = (p:Program {id: $program_id})-[:CALL|FLOW*1..%d]->(downstream:Program)
WITH DISTINCT downstream
RETURN downstream.id AS id, downstream.name AS name, downstream.source AS source
ORDER BY name;`, depth)
	return Statement{Query: query, Params: map[string]interface{}{"program_id": canonicalProgramID(programID)}}
}

// ImpactAnalysisQuery traverses upstream callers and flow predecessors for impact evaluation.
func ImpactAnalysisQuery(programID string, depth int) Statement {
	if depth <= 0 {
		depth = 5
	}
	query := fmt.Sprintf(`MATCH path = (p:Program {id: $program_id})<-[:CALL|FLOW*1..%d]-(upstream:Program)
WITH DISTINCT upstream
RETURN upstream.id AS id, upstream.name AS name, upstream.source AS source
ORDER BY name;`, depth)
	return Statement{Query: query, Params: map[string]interface{}{"program_id": canonicalProgramID(programID)}}
}

// RelatedProgramsQuery locates programs with shared IO assets or reciprocal calls.
func RelatedProgramsQuery(programID string, limit int) Statement {
	if limit <= 0 {
		limit = 10
	}
	query := fmt.Sprintf(`MATCH (target:Program {id: $program_id})
OPTIONAL MATCH (target)-[:INPUT|OUTPUT]->(asset:File)<-[:INPUT|OUTPUT]-(peer:Program)
WHERE peer.id <> target.id
WITH target, peer, COUNT(DISTINCT asset) AS shared_assets
OPTIONAL MATCH (target)-[:CALL]->(callee:Program {id: peer.id})
OPTIONAL MATCH (peer)-[:CALL]->(target)
WITH peer, shared_assets, COUNT(DISTINCT callee) + COUNT(DISTINCT target) AS call_strength
WHERE peer IS NOT NULL
RETURN peer.id AS id,
       peer.name AS name,
       shared_assets AS shared_io,
       call_strength AS call_links
ORDER BY shared_io DESC, call_links DESC, name ASC
LIMIT %d;`, limit)
	return Statement{Query: query, Params: map[string]interface{}{"program_id": canonicalProgramID(programID)}}
}

func (b *BatchBuilder) ensureProgram(id string) *ProgramNode {
	if existing, ok := b.programs[id]; ok {
		return existing
	}
	node := &ProgramNode{ID: id}
	b.programs[id] = node
	return node
}

func (b *BatchBuilder) ensureFile(id, name string) *FileNode {
	if existing, ok := b.files[id]; ok {
		if strings.TrimSpace(existing.Name) == "" {
			existing.Name = name
		}
		return existing
	}
	node := &FileNode{ID: id, Name: name, Path: name}
	b.files[id] = node
	return node
}

func (b *BatchBuilder) ensureFlowNode(id string) *FlowNode {
	if existing, ok := b.flowSteps[id]; ok {
		return existing
	}
	node := &FlowNode{ID: id}
	b.flowSteps[id] = node
	return node
}

func (b *BatchBuilder) ensureBusinessRule(id string) *BusinessRuleNode {
	if existing, ok := b.businessRules[id]; ok {
		return existing
	}
	node := &BusinessRuleNode{ID: id}
	b.businessRules[id] = node
	return node
}

func (b *BatchBuilder) ensureCallEdge(key, from, to string) *CallEdge {
	if existing, ok := b.calls[key]; ok {
		return existing
	}
	edge := &CallEdge{FromID: from, ToID: to}
	b.calls[key] = edge
	return edge
}

func (b *BatchBuilder) ensureInputEdge(key, programID, fileID string) *InputEdge {
	if existing, ok := b.inputs[key]; ok {
		return existing
	}
	edge := &InputEdge{ProgramID: programID, FileID: fileID}
	b.inputs[key] = edge
	return edge
}

func (b *BatchBuilder) ensureOutputEdge(key, programID, fileID string) *OutputEdge {
	if existing, ok := b.outputs[key]; ok {
		return existing
	}
	edge := &OutputEdge{ProgramID: programID, FileID: fileID}
	b.outputs[key] = edge
	return edge
}

func (b *BatchBuilder) ensureFlowEdge(key, programID, stepID string) *FlowEdge {
	if existing, ok := b.flows[key]; ok {
		return existing
	}
	edge := &FlowEdge{ProgramID: programID, StepID: stepID}
	b.flows[key] = edge
	return edge
}

func (b *BatchBuilder) ensureRuleLink(key, programID, ruleID string) *BusinessRuleEdge {
	if existing, ok := b.ruleLinks[key]; ok {
		return existing
	}
	edge := &BusinessRuleEdge{ProgramID: programID, RuleID: ruleID}
	b.ruleLinks[key] = edge
	return edge
}

func canonicalProgramID(name string) string {
	trimmed := strings.TrimSpace(name)
	if trimmed == "" {
		return ""
	}
	return strings.ToUpper(trimmed)
}

func canonicalFileID(name string) string {
	trimmed := strings.TrimSpace(name)
	if trimmed == "" {
		return ""
	}
	return slugify(trimmed)
}

func flowNodeID(docID, nodeID string) string {
	base := strings.TrimSpace(docID)
	if base == "" {
		base = "flow"
	}
	return base + ":" + slugify(nodeID)
}

func slugify(value string) string {
	value = strings.TrimSpace(strings.ToLower(value))
	if value == "" {
		return ""
	}
	var builder strings.Builder
	builder.Grow(len(value))
	var lastUnderscore bool
	for _, r := range value {
		switch {
		case unicode.IsLetter(r) || unicode.IsDigit(r):
			builder.WriteRune(r)
			lastUnderscore = false
		default:
			if !lastUnderscore {
				builder.WriteRune('_')
				lastUnderscore = true
			}
		}
	}
	result := strings.Trim(builder.String(), "_")
	if result == "" {
		return "asset"
	}
	return result
}

func appendUnique(base []string, values ...string) []string {
	existing := make(map[string]struct{}, len(base))
	for _, item := range base {
		trimmed := strings.TrimSpace(item)
		if trimmed == "" {
			continue
		}
		existing[strings.ToLower(trimmed)] = struct{}{}
	}
	for _, value := range values {
		trimmed := strings.TrimSpace(value)
		if trimmed == "" {
			continue
		}
		key := strings.ToLower(trimmed)
		if _, ok := existing[key]; ok {
			continue
		}
		existing[key] = struct{}{}
		base = append(base, trimmed)
	}
	return base
}

func firstLine(value string) string {
	trimmed := strings.TrimSpace(value)
	if trimmed == "" {
		return ""
	}
	if idx := strings.IndexAny(trimmed, "\r\n"); idx >= 0 {
		return strings.TrimSpace(trimmed[:idx])
	}
	return trimmed
}

func firstNonEmpty(values ...string) string {
	for _, value := range values {
		trimmed := strings.TrimSpace(value)
		if trimmed != "" {
			return trimmed
		}
	}
	return ""
}
