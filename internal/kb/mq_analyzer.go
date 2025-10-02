// File path: internal/kb/mq_analyzer.go
package kb

import (
	"context"
	"fmt"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
)

type mqAnalyzer struct{}

var mqOpRe = regexp.MustCompile(`(?i)MQ(PUT|GET|OPEN|CLOSE|CONN|DISC|INQ|SET|SUB|PUB)`) // MQ verbs

func (m *mqAnalyzer) Name() string { return "mq" }

func (m *mqAnalyzer) Match(path string, data []byte) bool {
	return mqOpRe.Match(data)
}

func (m *mqAnalyzer) Parse(ctx context.Context, path string, data []byte) ([]Doc, error) {
	calls := extractMQCalls(string(data))
	if len(calls) == 0 {
		return nil, nil
	}
	baseName := strings.TrimSuffix(filepath.Base(path), filepath.Ext(path))
	technologies := []string{"MQ"}
	queueSummary := summarizeMQQueues(calls)
	var baseLines []string
	baseLines = append(baseLines, fmt.Sprintf("MQ messaging summary for %s", baseName))
	for _, line := range queueSummary {
		baseLines = append(baseLines, "- "+line)
	}
	baseDoc := Doc{
		ID:           fmt.Sprintf("%s:mq", baseName),
		Program:      filepath.Base(path),
		SourcePath:   path,
		Chunk:        -50,
		Type:         "mq_flow",
		Content:      strings.Join(baseLines, "\n"),
		Summary:      fmt.Sprintf("MQ messaging interactions for %s", baseName),
		Technologies: technologies,
	}
	if baseDoc.Extra == nil {
		baseDoc.Extra = make(map[string]string)
	}
	if len(queueSummary) > 0 {
		baseDoc.Extra["queues"] = strings.Join(queueSummary, " | ")
	}

	flowLines := []string{fmt.Sprintf("MQ call flow for %s", baseName)}
	for idx, call := range calls {
		desc := describeMQCall(call)
		flowLines = append(flowLines, fmt.Sprintf("- Step %d: %s", idx+1, desc))
	}

	workingLines, descriptors, correlIDs := buildMQWorkingNotes(baseName, calls)
	if len(descriptors) > 0 {
		baseDoc.Extra["descriptors"] = strings.Join(descriptors, ", ")
	}
	if len(correlIDs) > 0 {
		baseDoc.Extra["correlation_ids"] = strings.Join(correlIDs, ", ")
	}

	businessLines := buildMQBusinessNotes(queueSummary, calls)

	docs := []Doc{baseDoc}
	if flow := strings.Join(nonEmpty(flowLines), "\n"); flow != "" {
		docs = append(docs, Doc{
			ID:           fmt.Sprintf("%s:mq:flow", baseName),
			Program:      filepath.Base(path),
			SourcePath:   path,
			Chunk:        -51,
			Type:         "flow",
			Content:      flow,
			Summary:      fmt.Sprintf("MQ flow for %s", baseName),
			Technologies: technologies,
		})
	}
	if working := strings.Join(nonEmpty(workingLines), "\n"); working != "" {
		docs = append(docs, Doc{
			ID:           fmt.Sprintf("%s:mq:working", baseName),
			Program:      filepath.Base(path),
			SourcePath:   path,
			Chunk:        -52,
			Type:         "working_notes",
			Content:      working,
			Summary:      fmt.Sprintf("MQ working notes for %s", baseName),
			Technologies: technologies,
			Extra:        baseDoc.Extra,
		})
	}
	if business := strings.Join(nonEmpty(businessLines), "\n"); business != "" {
		docs = append(docs, Doc{
			ID:           fmt.Sprintf("%s:mq:business", baseName),
			Program:      filepath.Base(path),
			SourcePath:   path,
			Chunk:        -53,
			Type:         "business_rules",
			Content:      business,
			Summary:      fmt.Sprintf("MQ business rules for %s", baseName),
			Technologies: technologies,
		})
	}

	return docs, nil
}

type mqCall struct {
	Verb       string
	Queue      string
	Descriptor string
	CorrelID   string
	Raw        string
}

func extractMQCalls(text string) []mqCall {
	lines := strings.Split(text, "\n")
	var calls []mqCall
	for i := 0; i < len(lines); i++ {
		line := lines[i]
		upper := strings.ToUpper(line)
		loc := mqOpRe.FindStringSubmatchIndex(upper)
		if loc == nil || len(loc) < 4 {
			continue
		}
		verb := strings.ToUpper(upper[loc[2]:loc[3]])
		statement := strings.TrimSpace(line[loc[0]:])
		combined := statement
		for j := i + 1; j < len(lines); j++ {
			next := strings.TrimSpace(lines[j])
			if next == "" {
				break
			}
			upperNext := strings.ToUpper(next)
			if mqOpRe.MatchString(upperNext) {
				break
			}
			if strings.HasPrefix(strings.TrimSpace(next), "*") {
				break
			}
			combined = combined + " " + next
			if strings.Contains(next, ".") || strings.Contains(next, ";") || strings.Contains(upperNext, "END-EXEC") {
				break
			}
		}
		attrs := parseMQAttributes(combined)
		calls = append(calls, mqCall{
			Verb:       verb,
			Queue:      attrs["queue"],
			Descriptor: attrs["descriptor"],
			CorrelID:   attrs["correlid"],
			Raw:        combined,
		})
	}
	return calls
}

func parseMQAttributes(statement string) map[string]string {
	attrs := make(map[string]string)
	if statement == "" {
		return attrs
	}
	queue := extractQueueName(statement)
	if queue != "" {
		attrs["queue"] = queue
	}
	if desc := extractFirstMatch(`(?i)MQMD[\w-]*\s*\(([^)]*)\)`, statement); desc != "" {
		attrs["descriptor"] = desc
	}
	if corr := extractFirstMatch(`(?i)CORRELID\s*\(([^)]*)\)`, statement); corr != "" {
		attrs["correlid"] = corr
	}
	return attrs
}

func extractQueueName(statement string) string {
	queueRe := regexp.MustCompile(`(?i)QUEUE\s*\(?\s*'?([A-Z0-9_.\-]+)'?\)?`)
	if match := queueRe.FindStringSubmatch(statement); len(match) > 1 {
		return strings.ToUpper(strings.TrimSpace(match[1]))
	}
	quotedRe := regexp.MustCompile(`'([A-Z0-9_.\-]+)'`)
	if match := quotedRe.FindStringSubmatch(statement); len(match) > 1 {
		return strings.ToUpper(strings.TrimSpace(match[1]))
	}
	tokens := strings.Fields(statement)
	if len(tokens) > 1 {
		candidate := strings.Trim(tokens[1], ",.")
		candidate = strings.Trim(candidate, "()")
		candidate = strings.Trim(candidate, "'")
		upper := strings.ToUpper(candidate)
		if upper != "" && !strings.Contains(upper, "HCONN") && !strings.Contains(upper, "HOBJ") && !strings.Contains(upper, "USING") && !strings.Contains(upper, "MQMD") {
			return upper
		}
	}
	return ""
}

func extractFirstMatch(pattern, statement string) string {
	re := regexp.MustCompile(pattern)
	match := re.FindStringSubmatch(statement)
	if len(match) > 1 {
		return strings.TrimSpace(strings.Trim(match[1], "'\""))
	}
	return ""
}

func summarizeMQQueues(calls []mqCall) []string {
	type queueInfo struct {
		producers []string
		consumers []string
		others    []string
	}
	details := make(map[string]*queueInfo)
	for _, call := range calls {
		queue := call.Queue
		if queue == "" {
			queue = "(unspecified)"
		}
		info, ok := details[queue]
		if !ok {
			info = &queueInfo{}
			details[queue] = info
		}
		descriptor := describeMQCall(call)
		switch call.Verb {
		case "PUT", "PUB":
			info.producers = append(info.producers, descriptor)
		case "GET", "SUB":
			info.consumers = append(info.consumers, descriptor)
		default:
			info.others = append(info.others, descriptor)
		}
	}
	if len(details) == 0 {
		return nil
	}
	var queues []string
	for queue := range details {
		queues = append(queues, queue)
	}
	sort.Strings(queues)
	var summary []string
	for _, queue := range queues {
		info := details[queue]
		var segments []string
		if len(info.producers) > 0 {
			segments = append(segments, fmt.Sprintf("produced by %s", strings.Join(info.producers, "; ")))
		}
		if len(info.consumers) > 0 {
			segments = append(segments, fmt.Sprintf("consumed by %s", strings.Join(info.consumers, "; ")))
		}
		if len(info.others) > 0 {
			segments = append(segments, fmt.Sprintf("operations %s", strings.Join(info.others, "; ")))
		}
		text := strings.Join(segments, "; ")
		if strings.TrimSpace(text) == "" {
			text = "MQ operations detected"
		}
		summary = append(summary, fmt.Sprintf("%s: %s", queue, text))
	}
	return summary
}

func describeMQCall(call mqCall) string {
	queue := call.Queue
	if queue == "" {
		queue = "unspecified queue"
	}
	extras := []string{}
	if call.Descriptor != "" {
		extras = append(extras, fmt.Sprintf("MQMD %s", call.Descriptor))
	}
	if call.CorrelID != "" {
		extras = append(extras, fmt.Sprintf("CorrelId %s", call.CorrelID))
	}
	detail := fmt.Sprintf("MQ%s to %s", call.Verb, queue)
	if call.Verb == "GET" || call.Verb == "SUB" {
		detail = fmt.Sprintf("MQ%s from %s", call.Verb, queue)
	}
	if len(extras) > 0 {
		detail = fmt.Sprintf("%s (%s)", detail, strings.Join(extras, ", "))
	}
	return detail
}

func buildMQWorkingNotes(baseName string, calls []mqCall) ([]string, []string, []string) {
	lines := []string{fmt.Sprintf("Working notes for %s", baseName)}
	var producers []string
	var consumers []string
	var descriptors []string
	var correlIDs []string
	for _, call := range calls {
		if call.Descriptor != "" {
			descriptors = append(descriptors, strings.ToUpper(call.Descriptor))
		}
		if call.CorrelID != "" {
			correlIDs = append(correlIDs, strings.ToUpper(call.CorrelID))
		}
		desc := describeMQCall(call)
		switch call.Verb {
		case "PUT", "PUB":
			producers = append(producers, desc)
		case "GET", "SUB":
			consumers = append(consumers, desc)
		}
	}
	if len(producers) > 0 {
		lines = append(lines, fmt.Sprintf("- Producer calls: %s", strings.Join(producers, "; ")))
	}
	if len(consumers) > 0 {
		lines = append(lines, fmt.Sprintf("- Consumer calls: %s", strings.Join(consumers, "; ")))
	}
	if dedup := uniqueStringsFromSlice(descriptors); len(dedup) > 0 {
		lines = append(lines, fmt.Sprintf("- Message descriptors observed: %s", strings.Join(dedup, ", ")))
		descriptors = dedup
	} else {
		descriptors = nil
	}
	if dedup := uniqueStringsFromSlice(correlIDs); len(dedup) > 0 {
		lines = append(lines, fmt.Sprintf("- Correlation identifiers used: %s", strings.Join(dedup, ", ")))
		correlIDs = dedup
	} else {
		correlIDs = nil
	}
	if len(lines) == 1 {
		lines = append(lines, "- Review MQ call parameters for context")
	}
	return lines, descriptors, correlIDs
}

func buildMQBusinessNotes(queueSummary []string, calls []mqCall) []string {
	lines := []string{"MQ integration considerations"}
	for _, summary := range queueSummary {
		lines = append(lines, "- "+summary)
	}
	var correlationInsights []string
	for _, call := range calls {
		if call.CorrelID != "" {
			correlationInsights = append(correlationInsights, fmt.Sprintf("Correlation %s managed via MQ%s", strings.ToUpper(call.CorrelID), call.Verb))
		}
	}
	if len(correlationInsights) > 0 {
		lines = append(lines, "- "+strings.Join(uniqueStringsFromSlice(correlationInsights), "; "))
	}
	if len(lines) == 1 {
		lines = append(lines, "- Ensure MQ interactions include producer and consumer mapping")
	}
	return lines
}
