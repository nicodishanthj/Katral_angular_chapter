// File path: internal/kb/flow_helpers.go
package kb

import (
	"fmt"
	"strings"

	"github.com/nicodishanthj/Katral_phase1/internal/kb/model"
)

func buildFlowContent(program string, inputs, outputs, calls []string, logic []*model.LogicStep) (string, *FlowDiagram) {
	var sections []string
	sections = append(sections, fmt.Sprintf("Program %s orchestrates the following flow:", program))
	if len(inputs) > 0 {
		sections = append(sections, fmt.Sprintf("- Ingests data from %s.", strings.Join(inputs, ", ")))
	}
	if len(outputs) > 0 {
		sections = append(sections, fmt.Sprintf("- Produces outputs to %s.", strings.Join(outputs, ", ")))
	}
	if len(calls) > 0 {
		sections = append(sections, fmt.Sprintf("- Invokes external routines: %s.", strings.Join(calls, ", ")))
	}
	if len(logic) > 0 {
		sections = append(sections, "Control flow overview:")
		sections = append(sections, describeFlow(logic, 0)...)
	}
	var diagram *FlowDiagram
	if len(logic) > 0 {
		diagram = buildFlowDiagram(program, logic)
	}
	if len(sections) <= 1 && diagram == nil {
		return "", nil
	}
	return strings.Join(sections, "\n"), diagram
}

func buildFlowDiagram(program string, logic []*model.LogicStep) *FlowDiagram {
	builder := newFlowDiagramBuilder()
	startLabel := fmt.Sprintf("Start %s", program)
	startID := builder.addNode(startLabel, "start")
	prev := startID
	for _, step := range logic {
		nodeID := builder.addStep(step)
		if nodeID == "" {
			continue
		}
		if prev == startID {
			builder.addEdge(startID, nodeID, "")
		} else {
			builder.addEdge(prev, nodeID, "")
		}
		prev = nodeID
	}
	if len(builder.nodes) <= 1 && len(builder.edges) == 0 {
		return nil
	}
	return &FlowDiagram{Nodes: builder.nodes, Edges: builder.edges}
}

type flowDiagramBuilder struct {
	nodes   []FlowNode
	edges   []FlowEdge
	counter int
}

func newFlowDiagramBuilder() *flowDiagramBuilder {
	return &flowDiagramBuilder{}
}

func (b *flowDiagramBuilder) addNode(label, typ string) string {
	b.counter++
	id := fmt.Sprintf("n%d", b.counter)
	trimmed := strings.TrimSpace(label)
	if trimmed == "" {
		trimmed = strings.TrimSpace(typ)
	}
	if trimmed == "" {
		trimmed = fmt.Sprintf("Step %d", b.counter)
	}
	b.nodes = append(b.nodes, FlowNode{ID: id, Label: trimmed, Type: typ})
	return id
}

func (b *flowDiagramBuilder) addEdge(from, to, label string) {
	if from == "" || to == "" {
		return
	}
	trimmed := strings.TrimSpace(label)
	b.edges = append(b.edges, FlowEdge{From: from, To: to, Label: trimmed})
}

func (b *flowDiagramBuilder) addStep(step *model.LogicStep) string {
	if step == nil {
		return ""
	}
	label := summarizeFlowStep(step)
	if strings.TrimSpace(label) == "" {
		label = step.Type
	}
	nodeID := b.addNode(label, step.Type)

	var prevChild string
	for idx, child := range step.Children {
		childID := b.addStep(child)
		if childID == "" {
			continue
		}
		if prevChild == "" {
			b.addEdge(nodeID, childID, labelForBranch(step, idx))
		} else {
			b.addEdge(prevChild, childID, "")
		}
		prevChild = childID
	}

	var prevElse string
	for idx, alt := range step.Else {
		elseID := b.addStep(alt)
		if elseID == "" {
			continue
		}
		if prevElse == "" {
			label := "else"
			if len(step.Else) > 1 {
				label = fmt.Sprintf("else %d", idx+1)
			}
			b.addEdge(nodeID, elseID, label)
		} else {
			b.addEdge(prevElse, elseID, "")
		}
		prevElse = elseID
	}

	return nodeID
}

func labelForBranch(step *model.LogicStep, idx int) string {
	switch step.Type {
	case "if", "when", "evaluate":
		if len(step.Children) > 1 {
			return fmt.Sprintf("then %d", idx+1)
		}
		return "then"
	case "perform":
		if idx == 0 && strings.TrimSpace(step.Condition) != "" {
			return "loop"
		}
	}
	return ""
}

func buildBusinessRules(program string, logic []*model.LogicStep) string {
	if len(logic) == 0 {
		return ""
	}
	var builder strings.Builder
	builder.WriteString("Business rules inferred for ")
	builder.WriteString(program)
	builder.WriteString(":\n")
	for _, line := range describeBusinessRules(logic, 0) {
		if strings.TrimSpace(line) == "" {
			continue
		}
		builder.WriteString(line)
		builder.WriteString("\n")
	}
	return strings.TrimSuffix(builder.String(), "\n")
}

func describeFlow(steps []*model.LogicStep, depth int) []string {
	indent := strings.Repeat("  ", depth)
	var lines []string
	for _, step := range steps {
		summary := summarizeFlowStep(step)
		if summary != "" {
			lines = append(lines, fmt.Sprintf("%s- %s", indent, summary))
		}
		if len(step.Children) > 0 {
			lines = append(lines, describeFlow(step.Children, depth+1)...)
		}
		if len(step.Else) > 0 {
			lines = append(lines, fmt.Sprintf("%s  Else:", indent))
			lines = append(lines, describeFlow(step.Else, depth+1)...)
		}
	}
	return lines
}

func summarizeFlowStep(step *model.LogicStep) string {
	switch step.Type {
	case "if":
		return fmt.Sprintf("If %s", step.Condition)
	case "when":
		return fmt.Sprintf("When %s", step.Condition)
	case "evaluate":
		return fmt.Sprintf("Evaluate %s", step.Condition)
	case "perform":
		if step.Condition != "" {
			return fmt.Sprintf("Perform %s until %s", strings.Join(step.Targets, " to "), step.Condition)
		}
		return fmt.Sprintf("Perform %s", strings.Join(step.Targets, " to "))
	case "move":
		from := step.Data["from"]
		to := step.Data["to"]
		if from != "" && to != "" {
			return fmt.Sprintf("Move %s to %s", from, to)
		}
		return step.Action
	case "read":
		return fmt.Sprintf("Read %s", strings.Join(step.Targets, ", "))
	case "write":
		return fmt.Sprintf("Write %s", strings.Join(step.Targets, ", "))
	case "statement":
		return step.Action
	default:
		if step.Action != "" {
			return step.Action
		}
		return step.Type
	}
}

func describeBusinessRules(steps []*model.LogicStep, depth int) []string {
	indent := strings.Repeat("  ", depth)
	var lines []string
	for _, step := range steps {
		switch step.Type {
		case "if":
			lines = append(lines, fmt.Sprintf("%s- If %s then:", indent, step.Condition))
			if len(step.Children) > 0 {
				lines = append(lines, describeBusinessRules(step.Children, depth+1)...)
			}
			if len(step.Else) > 0 {
				lines = append(lines, fmt.Sprintf("%s  Else:", indent))
				lines = append(lines, describeBusinessRules(step.Else, depth+1)...)
			}
		case "evaluate":
			lines = append(lines, fmt.Sprintf("%s- Evaluate %s:", indent, step.Condition))
			if len(step.Children) > 0 {
				lines = append(lines, describeBusinessRules(step.Children, depth+1)...)
			}
		case "when":
			lines = append(lines, fmt.Sprintf("%s- When %s:", indent, step.Condition))
			if len(step.Children) > 0 {
				lines = append(lines, describeBusinessRules(step.Children, depth+1)...)
			}
		case "perform":
			clause := strings.Join(step.Targets, " to ")
			if step.Condition != "" {
				lines = append(lines, fmt.Sprintf("%s- Perform %s until %s.", indent, clause, step.Condition))
			} else {
				lines = append(lines, fmt.Sprintf("%s- Perform %s.", indent, clause))
			}
		case "move":
			from := step.Data["from"]
			to := step.Data["to"]
			if from != "" && to != "" {
				lines = append(lines, fmt.Sprintf("%s- Move %s to %s.", indent, from, to))
			} else if step.Action != "" {
				lines = append(lines, fmt.Sprintf("%s- %s.", indent, step.Action))
			}
		case "read":
			lines = append(lines, fmt.Sprintf("%s- Read %s.", indent, strings.Join(step.Targets, ", ")))
		case "write":
			lines = append(lines, fmt.Sprintf("%s- Write %s.", indent, strings.Join(step.Targets, ", ")))
		default:
			if step.Action != "" {
				lines = append(lines, fmt.Sprintf("%s- %s.", indent, step.Action))
			}
		}
	}
	return lines
}
