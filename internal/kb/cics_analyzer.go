// File path: internal/kb/cics_analyzer.go
package kb

import (
	"context"
	"fmt"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
	"unicode"
)

type cicsAnalyzer struct{}

var (
	cicsBlockRe = regexp.MustCompile(`(?is)EXEC\s+CICS\s+([A-Z]+)(.*?)(?:END-EXEC|\.)`)
)

func (c *cicsAnalyzer) Name() string { return "cics" }

func (c *cicsAnalyzer) Match(path string, data []byte) bool {
	if cicsBlockRe.Match(data) {
		return true
	}
	content := strings.ToUpper(string(data))
	return strings.Contains(content, "EXEC CICS")
}

func (c *cicsAnalyzer) Parse(ctx context.Context, path string, data []byte) ([]Doc, error) {
	raw := string(data)
	matches := cicsBlockRe.FindAllStringSubmatch(raw, -1)
	if len(matches) == 0 {
		return nil, nil
	}
	type cicsCommand struct {
		verb  string
		attrs map[string]string
		flags []string
		raw   string
	}

	var commands []cicsCommand
	for _, match := range matches {
		if len(match) < 3 {
			continue
		}
		verb := strings.ToUpper(strings.TrimSpace(match[1]))
		if verb == "" {
			continue
		}
		operands := strings.TrimSpace(match[2])
		attrs, flags := parseCICSOperands(operands)
		commands = append(commands, cicsCommand{
			verb:  verb,
			attrs: attrs,
			flags: flags,
			raw:   strings.TrimSpace(match[0]),
		})
	}
	if len(commands) == 0 {
		return nil, nil
	}

	baseName := strings.TrimSuffix(filepath.Base(path), filepath.Ext(path))
	idBase := pathToIDFragment(path)
	summaryLines := []string{fmt.Sprintf("CICS command summary for %s", baseName)}
	var commandDetails []string
	resourceSets := map[string][]string{
		"MAP":      {},
		"MAPSET":   {},
		"PROGRAM":  {},
		"TRANSID":  {},
		"CHANNEL":  {},
		"COMMAREA": {},
		"QUEUE":    {},
	}

	for idx, cmd := range commands {
		var detailParts []string
		keys := make([]string, 0, len(cmd.attrs))
		for k := range cmd.attrs {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		for _, key := range keys {
			val := strings.TrimSpace(cmd.attrs[key])
			if val != "" {
				detailParts = append(detailParts, fmt.Sprintf("%s=%s", key, val))
				if _, ok := resourceSets[key]; ok {
					resourceSets[key] = append(resourceSets[key], val)
				}
			} else {
				detailParts = append(detailParts, key)
			}
		}
		if len(cmd.flags) > 0 {
			detailParts = append(detailParts, strings.Join(cmd.flags, ", "))
		}
		line := fmt.Sprintf("- %s", cmd.verb)
		if len(detailParts) > 0 {
			line = fmt.Sprintf("%s: %s", line, strings.Join(detailParts, ", "))
		}
		summaryLines = append(summaryLines, line)

		flowDetails := detailParts
		if len(flowDetails) == 0 {
			flowDetails = []string{"No explicit operands"}
		}
		commandDetails = append(commandDetails, fmt.Sprintf("Step %d: %s (%s)", idx+1, cmd.verb, strings.Join(flowDetails, ", ")))
	}

	technologies := []string{"CICS"}
	baseDoc := Doc{
		ID:           fmt.Sprintf("%s:cics", idBase),
		Program:      filepath.Base(path),
		SourcePath:   path,
		Chunk:        -40,
		Type:         "cics_flow",
		Content:      strings.Join(summaryLines, "\n"),
		Summary:      fmt.Sprintf("CICS commands with operands for %s", baseName),
		Technologies: technologies,
	}

	if baseDoc.Extra == nil {
		baseDoc.Extra = make(map[string]string)
	}
	var detailStrings []string
	for _, cmd := range commands {
		parts := []string{cmd.verb}
		keys := make([]string, 0, len(cmd.attrs))
		for k := range cmd.attrs {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		for _, key := range keys {
			val := strings.TrimSpace(cmd.attrs[key])
			if val != "" {
				parts = append(parts, fmt.Sprintf("%s=%s", key, val))
			} else {
				parts = append(parts, key)
			}
		}
		if len(cmd.flags) > 0 {
			parts = append(parts, strings.Join(cmd.flags, ","))
		}
		detailStrings = append(detailStrings, strings.Join(parts, " "))
	}
	baseDoc.Extra["commands"] = strings.Join(detailStrings, " | ")
	for resource, values := range resourceSets {
		collapsed := strings.Join(uniqueStringsFromSlice(values), ", ")
		if collapsed != "" {
			baseDoc.Extra[strings.ToLower(resource)] = collapsed
		}
	}

	flowLines := []string{fmt.Sprintf("CICS flow for %s", baseName)}
	for _, detail := range commandDetails {
		flowLines = append(flowLines, "- "+detail)
	}

	workingLines := []string{fmt.Sprintf("Working notes for %s", baseName)}
	if maps := baseDoc.Extra["map"]; maps != "" {
		workingLines = append(workingLines, fmt.Sprintf("- Maps referenced: %s", maps))
	}
	if mapsets := baseDoc.Extra["mapset"]; mapsets != "" {
		workingLines = append(workingLines, fmt.Sprintf("- Mapsets referenced: %s", mapsets))
	}
	if progs := baseDoc.Extra["program"]; progs != "" {
		workingLines = append(workingLines, fmt.Sprintf("- Linked programs: %s", progs))
	}
	if trans := baseDoc.Extra["transid"]; trans != "" {
		workingLines = append(workingLines, fmt.Sprintf("- Transactions invoked: %s", trans))
	}
	if chans := baseDoc.Extra["channel"]; chans != "" {
		workingLines = append(workingLines, fmt.Sprintf("- Channels used: %s", chans))
	}
	if comms := baseDoc.Extra["commarea"]; comms != "" {
		workingLines = append(workingLines, fmt.Sprintf("- COMMAREAs referenced: %s", comms))
	}

	docs := []Doc{baseDoc}
	if flow := strings.Join(nonEmpty(flowLines), "\n"); flow != "" {
		docs = append(docs, Doc{
			ID:           fmt.Sprintf("%s:cics:flow", idBase),
			Program:      filepath.Base(path),
			SourcePath:   path,
			Chunk:        -41,
			Type:         "flow",
			Content:      flow,
			Summary:      fmt.Sprintf("CICS flow for %s", baseName),
			Technologies: technologies,
		})
	}
	if working := strings.Join(nonEmpty(workingLines), "\n"); working != "" {
		docs = append(docs, Doc{
			ID:           fmt.Sprintf("%s:cics:working", idBase),
			Program:      filepath.Base(path),
			SourcePath:   path,
			Chunk:        -42,
			Type:         "working_notes",
			Content:      working,
			Summary:      fmt.Sprintf("CICS working notes for %s", baseName),
			Technologies: technologies,
			Extra:        baseDoc.Extra,
		})
	}

	return docs, nil
}

func parseCICSOperands(text string) (map[string]string, []string) {
	attrs := make(map[string]string)
	var flags []string
	cleaned := strings.TrimSpace(strings.ReplaceAll(strings.ReplaceAll(text, "\n", " "), "\t", " "))
	if cleaned == "" {
		return attrs, flags
	}
	runes := []rune(cleaned)
	for i := 0; i < len(runes); {
		ch := runes[i]
		if unicode.IsSpace(ch) || ch == ',' || ch == '.' {
			i++
			continue
		}
		start := i
		for i < len(runes) && (unicode.IsLetter(runes[i]) || unicode.IsDigit(runes[i]) || runes[i] == '-' || runes[i] == '_') {
			i++
		}
		token := strings.ToUpper(strings.TrimSpace(string(runes[start:i])))
		if token == "" {
			i++
			continue
		}
		for i < len(runes) && unicode.IsSpace(runes[i]) {
			i++
		}
		if i < len(runes) && runes[i] == '(' {
			i++
			depth := 1
			valStart := i
			for i < len(runes) && depth > 0 {
				switch runes[i] {
				case '(':
					depth++
				case ')':
					depth--
					if depth == 0 {
						break
					}
				}
				i++
			}
			valEnd := i
			value := strings.TrimSpace(string(runes[valStart:valEnd]))
			value = strings.Trim(value, "'\" )(")
			attrs[token] = value
			if i < len(runes) && runes[i] == ')' {
				i++
			}
		} else {
			token = strings.Trim(token, ",.")
			if token != "" {
				flags = append(flags, token)
			}
		}
		for i < len(runes) && (unicode.IsSpace(runes[i]) || runes[i] == ',' || runes[i] == '.') {
			i++
		}
	}
	return attrs, flags
}
