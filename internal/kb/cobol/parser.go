// File path: internal/kb/cobol/parser.go
package cobol

import (
	"context"
	"encoding/json"
	"fmt"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/nicodishanthj/Katral_phase1/internal/kb/model"
)

var (
	programIDRe = regexp.MustCompile(`(?i)PROGRAM-ID\.\s*([A-Z0-9-]+)`)
	fdRe        = regexp.MustCompile(`(?i)\b(FD|SD)\s+([A-Z0-9-]+)`)
	callRe      = regexp.MustCompile(`(?i)\bCALL\s+'?([A-Z0-9-]+)'?`)
	paragraphRe = regexp.MustCompile(`(?m)^[ \t]*([A-Z0-9-]+)\.`)
	procedureRe = regexp.MustCompile(`(?is)PROCEDURE DIVISION\.(.*)`)
	divisionRe  = regexp.MustCompile(`(?im)^[ \t]*([A-Z-]+) DIVISION\.`)
)

type Document struct {
	ID         string
	Program    string
	SourcePath string
	Chunk      int
	Type       string
	Content    string
	Summary    string
	Inputs     []string
	Outputs    []string
	Calls      []string
	Paragraphs []string
	Logic      []string
	LogicTree  []*model.LogicStep
	Extra      map[string]string
}

type Parser struct {
	ChunkSize int
}

func NewParser() *Parser {
	return &Parser{ChunkSize: 40}
}

func (p *Parser) Parse(ctx context.Context, path string, data []byte) ([]Document, error) {
	if ctx != nil {
		select {
		case <-ctx.Done():
			return nil, ctx.Err()
		default:
		}
	}
	program := matchFirst(programIDRe, string(data))
	if program == "" {
		program = strings.ToUpper(strings.TrimSuffix(filepath.Base(path), filepath.Ext(path)))
	}
	lines := strings.Split(string(data), "\n")
	chunkDocs := chunkLines(program, path, lines, p.ChunkSize)

	inputs, outputs := parseFileDescriptors(string(data))
	calls := uniqueStrings(callRe.FindAllStringSubmatch(string(data), -1), 1)
	paragraphs := uniqueStrings(paragraphRe.FindAllStringSubmatch(string(data), -1), 1)
	logic, logicTree, paragraphSummaries := extractProcedureLogic(string(data))
	divisions := uniqueStrings(divisionRe.FindAllStringSubmatch(string(data), -1), 1)

	metaDoc := Document{
		ID:         fmt.Sprintf("%s:metadata", program),
		Program:    program,
		SourcePath: path,
		Chunk:      -1,
		Type:       "metadata",
		Content:    fmt.Sprintf("Program %s in %s", program, path),
		Summary:    summarizeProgram(program, inputs, outputs, calls),
		Inputs:     inputs,
		Outputs:    outputs,
		Calls:      calls,
		Paragraphs: paragraphs,
		Logic:      logic,
		LogicTree:  logicTree,
		Extra: map[string]string{
			"divisions": strings.Join(divisions, ", "),
		},
	}
	if len(paragraphSummaries) > 0 {
		if data, err := json.Marshal(paragraphSummaries); err == nil {
			metaDoc.Extra["paragraph_logic"] = string(data)
		}
	}
	docs := append([]Document{metaDoc}, chunkDocs...)
	return docs, nil
}

func chunkLines(program, source string, lines []string, size int) []Document {
	if size <= 0 {
		size = 40
	}
	var docs []Document
	var buffer []string
	chunkIndex := 0
	flush := func() {
		if len(buffer) == 0 {
			return
		}
		docs = append(docs, Document{
			ID:         fmt.Sprintf("%s:%d", strings.TrimSpace(program), chunkIndex),
			Program:    program,
			SourcePath: source,
			Chunk:      chunkIndex,
			Type:       "chunk",
			Content:    strings.Join(buffer, "\n"),
		})
		chunkIndex++
		buffer = buffer[:0]
	}
	for _, line := range lines {
		buffer = append(buffer, line)
		if len(buffer) >= size {
			flush()
		}
	}
	flush()
	return docs
}

func parseFileDescriptors(content string) (inputs, outputs []string) {
	matches := fdRe.FindAllStringSubmatch(content, -1)
	for _, m := range matches {
		name := strings.ToUpper(strings.TrimSpace(m[2]))
		if name == "" {
			continue
		}
		if strings.HasSuffix(name, "-FILE") || strings.HasSuffix(name, "-IN") {
			inputs = append(inputs, name)
		} else if strings.HasSuffix(name, "-OUT") || strings.HasSuffix(name, "-REPORT") {
			outputs = append(outputs, name)
		} else {
			inputs = append(inputs, name)
		}
	}
	inputs = dedupe(inputs)
	outputs = dedupe(outputs)
	return inputs, outputs
}

func extractProcedureLogic(content string) ([]string, []*model.LogicStep, map[string][]string) {
	matches := procedureRe.FindStringSubmatch(content)
	if len(matches) < 2 {
		return nil, nil, nil
	}
	body := matches[1]
	lines := strings.Split(body, "\n")
	var logic []string
	var steps []*model.LogicStep
	paragraphSummaries := make(map[string][]string)
	var stack []*logicContext
	currentParagraph := ""
	for _, rawLine := range lines {
		line := strings.TrimSpace(rawLine)
		if line == "" {
			continue
		}
		upper := strings.ToUpper(line)
		if strings.HasPrefix(upper, "*") {
			continue
		}
		if paragraph, remainder := extractParagraph(line); paragraph != "" {
			currentParagraph = paragraph
			line = remainder
			if line == "" {
				continue
			}
		}
		step, directive := parseLogicLine(line, currentParagraph)
		if directive != nil {
			stack = applyDirective(stack, directive)
			if step == nil {
				continue
			}
		}
		if step == nil {
			continue
		}
		if step.Type == "when" {
			stack = closeCaseContext(stack)
		}
		step.Raw = line
		if step.Paragraph == "" {
			step.Paragraph = currentParagraph
		}
		summary := summarizeStep(step)
		if step.Paragraph != "" && summary != "" {
			paragraphSummaries[step.Paragraph] = append(paragraphSummaries[step.Paragraph], summary)
		}
		if summary != "" {
			logic = append(logic, summary)
			if len(logic) > 20 {
				logic = logic[:20]
			}
		}
		stack, steps = appendStep(stack, steps, step)
	}
	return logic, steps, paragraphSummaries
}

type logicContext struct {
	step   *model.LogicStep
	branch string
}

type logicDirective struct {
	action string
}

var (
	paragraphLineRe = regexp.MustCompile(`(?i)^([A-Z0-9-]+)\.(.*)$`)
	ifRe            = regexp.MustCompile(`(?i)^IF\s+(.*)`)
	elseRe          = regexp.MustCompile(`(?i)^ELSE\b`)
	endIfRe         = regexp.MustCompile(`(?i)^END-IF\b`)
	evaluateRe      = regexp.MustCompile(`(?i)^EVALUATE\s+(.*)`)
	whenRe          = regexp.MustCompile(`(?i)^WHEN\s+(.*)`)
	endEvaluateRe   = regexp.MustCompile(`(?i)^END-EVALUATE\b`)
	performRe       = regexp.MustCompile(`(?i)^PERFORM\s+([A-Z0-9-]+)(?:\s+THRU\s+([A-Z0-9-]+))?(?:\s+UNTIL\s+(.+))?`)
	moveRe          = regexp.MustCompile(`(?i)^MOVE\s+(.+?)\s+TO\s+([A-Z0-9-]+)`)
	readWriteRe     = regexp.MustCompile(`(?i)^(READ|WRITE)\s+([A-Z0-9-]+)`)
)

func extractParagraph(line string) (string, string) {
	match := paragraphLineRe.FindStringSubmatch(line)
	if len(match) < 2 {
		return "", line
	}
	paragraph := strings.ToUpper(strings.TrimSpace(match[1]))
	remainder := strings.TrimSpace(match[2])
	return paragraph, remainder
}

func parseLogicLine(line, currentParagraph string) (*model.LogicStep, *logicDirective) {
	normalized := strings.TrimSuffix(line, ".")
	normalized = strings.TrimSpace(normalized)
	if normalized == "" {
		return nil, nil
	}
	if ifMatch := ifRe.FindStringSubmatch(normalized); len(ifMatch) > 1 {
		return &model.LogicStep{Type: "if", Condition: strings.TrimSpace(ifMatch[1]), Paragraph: currentParagraph}, nil
	}
	if elseRe.MatchString(normalized) {
		return nil, &logicDirective{action: "else"}
	}
	if endIfRe.MatchString(normalized) {
		return nil, &logicDirective{action: "pop_if"}
	}
	if evalMatch := evaluateRe.FindStringSubmatch(normalized); len(evalMatch) > 1 {
		return &model.LogicStep{Type: "evaluate", Condition: strings.TrimSpace(evalMatch[1]), Paragraph: currentParagraph}, nil
	}
	if whenMatch := whenRe.FindStringSubmatch(normalized); len(whenMatch) > 1 {
		return &model.LogicStep{Type: "when", Condition: strings.TrimSpace(whenMatch[1]), Paragraph: currentParagraph}, nil
	}
	if endEvaluateRe.MatchString(normalized) {
		return nil, &logicDirective{action: "pop_evaluate"}
	}
	if performMatch := performRe.FindStringSubmatch(normalized); len(performMatch) > 0 {
		targets := []string{strings.ToUpper(strings.TrimSpace(performMatch[1]))}
		if performMatch[2] != "" {
			targets = append(targets, strings.ToUpper(strings.TrimSpace(performMatch[2])))
		}
		condition := strings.TrimSpace(performMatch[3])
		return &model.LogicStep{Type: "perform", Targets: targets, Condition: condition, Paragraph: currentParagraph, Action: normalized}, nil
	}
	if moveMatch := moveRe.FindStringSubmatch(normalized); len(moveMatch) > 2 {
		source := strings.TrimSpace(moveMatch[1])
		dest := strings.ToUpper(strings.TrimSpace(moveMatch[2]))
		return &model.LogicStep{Type: "move", Data: map[string]string{"from": source, "to": dest}, Action: normalized, Paragraph: currentParagraph}, nil
	}
	if rwMatch := readWriteRe.FindStringSubmatch(normalized); len(rwMatch) > 2 {
		typ := strings.ToLower(strings.TrimSpace(rwMatch[1]))
		target := strings.ToUpper(strings.TrimSpace(rwMatch[2]))
		stepType := "read"
		if typ == "write" {
			stepType = "write"
		}
		return &model.LogicStep{Type: stepType, Targets: []string{target}, Action: normalized, Paragraph: currentParagraph}, nil
	}
	return &model.LogicStep{Type: "statement", Action: normalized, Paragraph: currentParagraph}, nil
}

func applyDirective(stack []*logicContext, directive *logicDirective) []*logicContext {
	if directive == nil {
		return stack
	}
	switch directive.action {
	case "else":
		for i := len(stack) - 1; i >= 0; i-- {
			if stack[i].step.Type == "if" {
				stack = stack[:i+1]
				stack[i].branch = "else"
				break
			}
		}
	case "pop_if":
		for len(stack) > 0 {
			top := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			if top.step.Type == "if" {
				break
			}
		}
	case "pop_evaluate":
		for len(stack) > 0 {
			top := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			if top.step.Type == "evaluate" {
				break
			}
		}
	}
	return stack
}

func closeCaseContext(stack []*logicContext) []*logicContext {
	for len(stack) > 0 {
		top := stack[len(stack)-1]
		if top.step.Type != "when" {
			break
		}
		stack = stack[:len(stack)-1]
	}
	return stack
}

func appendStep(stack []*logicContext, steps []*model.LogicStep, step *model.LogicStep) ([]*logicContext, []*model.LogicStep) {
	if step == nil {
		return stack, steps
	}
	var parent *logicContext
	if len(stack) > 0 {
		parent = stack[len(stack)-1]
	}
	switch {
	case parent == nil:
		steps = append(steps, step)
	case parent.branch == "else":
		parent.step.Else = append(parent.step.Else, step)
	default:
		parent.step.Children = append(parent.step.Children, step)
	}

	switch step.Type {
	case "if":
		stack = append(stack, &logicContext{step: step, branch: "then"})
	case "evaluate":
		stack = append(stack, &logicContext{step: step, branch: "evaluate"})
	case "when":
		stack = append(stack, &logicContext{step: step, branch: "case"})
	}
	return stack, steps
}

func summarizeStep(step *model.LogicStep) string {
	if step == nil {
		return ""
	}
	switch step.Type {
	case "if":
		return fmt.Sprintf("IF %s", step.Condition)
	case "when":
		return fmt.Sprintf("WHEN %s", step.Condition)
	case "evaluate":
		return fmt.Sprintf("EVALUATE %s", step.Condition)
	case "perform":
		target := strings.Join(step.Targets, " THRU ")
		if step.Condition != "" {
			return fmt.Sprintf("PERFORM %s UNTIL %s", target, step.Condition)
		}
		return fmt.Sprintf("PERFORM %s", target)
	case "move":
		from := step.Data["from"]
		to := step.Data["to"]
		if from != "" && to != "" {
			return fmt.Sprintf("MOVE %s TO %s", from, to)
		}
		return step.Action
	case "read":
		if len(step.Targets) > 0 {
			return fmt.Sprintf("READ %s", step.Targets[0])
		}
		return step.Action
	case "write":
		if len(step.Targets) > 0 {
			return fmt.Sprintf("WRITE %s", step.Targets[0])
		}
		return step.Action
	default:
		return step.Action
	}
}

func summarizeProgram(program string, inputs, outputs, calls []string) string {
	builder := &strings.Builder{}
	builder.WriteString("Program ")
	builder.WriteString(program)
	if len(inputs) > 0 {
		builder.WriteString(" reads ")
		builder.WriteString(strings.Join(inputs, ", "))
	}
	if len(outputs) > 0 {
		if len(inputs) > 0 {
			builder.WriteString(" and")
		}
		builder.WriteString(" writes ")
		builder.WriteString(strings.Join(outputs, ", "))
	}
	if len(calls) > 0 {
		builder.WriteString("; calls: ")
		builder.WriteString(strings.Join(calls, ", "))
	}
	return builder.String()
}

func uniqueStrings(matches [][]string, idx int) []string {
	var values []string
	for _, m := range matches {
		if len(m) <= idx {
			continue
		}
		v := strings.ToUpper(strings.TrimSpace(m[idx]))
		if v == "" {
			continue
		}
		values = append(values, v)
	}
	return dedupe(values)
}

func dedupe(values []string) []string {
	seen := make(map[string]struct{})
	var out []string
	for _, v := range values {
		if _, ok := seen[v]; ok {
			continue
		}
		seen[v] = struct{}{}
		out = append(out, v)
	}
	return out
}

func matchFirst(re *regexp.Regexp, input string) string {
	match := re.FindStringSubmatch(input)
	if len(match) < 2 {
		return ""
	}
	return strings.ToUpper(strings.TrimSpace(match[1]))
}
