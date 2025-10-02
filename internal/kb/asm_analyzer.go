// File path: internal/kb/asm_analyzer.go
package kb

import (
	"context"
	"fmt"
	"path/filepath"
	"regexp"
	"sort"
	"strconv"
	"strings"
)

type asmAnalyzer struct{}

var (
	asmRe           = regexp.MustCompile(`(?im)^[A-Z0-9@#]+\s+(CSECT|START|ENTRY|USING|DC|DS)`)
	asmRegisterRe   = regexp.MustCompile(`R([0-9]{1,2})`)
	asmPrimitiveOps = map[string]struct{}{
		"CSECT": {},
		"START": {},
		"ENTRY": {},
		"USING": {},
		"DROP":  {},
		"DC":    {},
		"DS":    {},
		"DSECT": {},
		"EQU":   {},
		"END":   {},
		"MVC":   {},
		"L":     {},
		"LA":    {},
		"ST":    {},
		"SR":    {},
		"AR":    {},
		"LR":    {},
		"B":     {},
		"BE":    {},
		"BNE":   {},
		"BAL":   {},
		"BR":    {},
	}
)

func (a *asmAnalyzer) Name() string { return "asm" }

func (a *asmAnalyzer) Match(path string, data []byte) bool {
	if asmRe.Match(data) {
		return true
	}
	lower := strings.ToLower(path)
	return strings.HasSuffix(lower, ".asm") || strings.HasSuffix(lower, ".mac")
}

func (a *asmAnalyzer) Parse(ctx context.Context, path string, data []byte) ([]Doc, error) {
	text := string(data)
	if !asmRe.MatchString(text) {
		return nil, nil
	}
	module := strings.ToUpper(strings.TrimSuffix(filepath.Base(path), filepath.Ext(path)))
	lines := strings.Split(text, "\n")
	var flowSteps []string
	var operations []string
	var working []string
	var business []string
	seenOps := make(map[string]struct{})
	registerUsage := make(map[string][]string)
	baseRegisters := []string{}
	storageDefs := []string{}
	macroSeen := make(map[string]struct{})
	macros := []string{}

	for _, raw := range lines {
		trimmed := strings.TrimSpace(raw)
		if trimmed == "" {
			continue
		}
		upper := strings.ToUpper(trimmed)
		if strings.HasPrefix(upper, "*") {
			note := strings.TrimSpace(strings.TrimPrefix(trimmed, "*"))
			if note != "" {
				business = append(business, note)
			}
			continue
		}
		parts := strings.Fields(upper)
		if len(parts) == 0 {
			continue
		}
		hasLeadingSpace := len(raw) > 0 && (raw[0] == ' ' || raw[0] == '\t')
		label := module
		var op string
		if !hasLeadingSpace && len(parts) > 1 {
			label = parts[0]
			op = parts[1]
		} else {
			op = parts[0]
		}
		if op == "" {
			continue
		}
		operandText := ""
		upperTrimmed := strings.ToUpper(trimmed)
		opIndex := strings.Index(upperTrimmed, op)
		if opIndex >= 0 {
			operandStart := opIndex + len(op)
			if operandStart < len(trimmed) {
				operandText = strings.TrimSpace(trimmed[operandStart:])
			}
		}
		upperOperands := strings.ToUpper(operandText)
		if _, ok := seenOps[op]; !ok {
			seenOps[op] = struct{}{}
			if operandText != "" {
				operations = append(operations, strings.TrimSpace(op+" "+operandText))
			} else {
				operations = append(operations, op)
			}
		}
		flowInstruction := strings.TrimSpace(op + " " + operandText)
		flowSteps = append(flowSteps, strings.TrimSpace(fmt.Sprintf("%s %s", label, flowInstruction)))
		if registers := asmRegisterRe.FindAllString(upperOperands, -1); len(registers) > 0 {
			context := strings.TrimSpace(fmt.Sprintf("%s %s", label, op))
			for _, reg := range registers {
				usage := registerUsage[reg]
				usage = append(usage, context)
				registerUsage[reg] = usage
			}
		}
		switch op {
		case "CSECT", "START", "ENTRY":
			business = append(business, fmt.Sprintf("%s defines control flow", op))
		case "USING":
			working = append(working, fmt.Sprintf("%s establishes base register", label))
			context := strings.TrimSpace(fmt.Sprintf("%s %s", label, op))
			if regs := asmRegisterRe.FindAllString(upperOperands, -1); len(regs) > 0 {
				baseRegisters = append(baseRegisters, regs...)
				for _, reg := range regs {
					usage := registerUsage[reg]
					usage = append(usage, context)
					registerUsage[reg] = usage
				}
			} else if idx := strings.LastIndex(operandText, ","); idx != -1 {
				candidate := normalizeAssemblerRegister(operandText[idx+1:])
				if candidate != "" {
					baseRegisters = append(baseRegisters, candidate)
					usage := registerUsage[candidate]
					usage = append(usage, context)
					registerUsage[candidate] = usage
				}
			}
		case "DC", "DS":
			working = append(working, fmt.Sprintf("%s reserves storage", label))
			if operandText != "" {
				storageDefs = append(storageDefs, fmt.Sprintf("%s %s %s", label, op, operandText))
			} else {
				storageDefs = append(storageDefs, fmt.Sprintf("%s %s", label, op))
			}
		}
		if op == "DSECT" {
			if operandText != "" {
				storageDefs = append(storageDefs, fmt.Sprintf("%s %s %s", label, op, operandText))
			} else {
				storageDefs = append(storageDefs, fmt.Sprintf("%s %s", label, op))
			}
		}
		if _, primitive := asmPrimitiveOps[op]; !primitive {
			macroKey := op
			if operandText != "" {
				macroKey = strings.TrimSpace(op + " " + operandText)
			}
			if _, seen := macroSeen[macroKey]; !seen {
				macroSeen[macroKey] = struct{}{}
				macros = append(macros, macroKey)
			}
		}
	}
	if len(flowSteps) == 0 {
		return nil, nil
	}
	technologies := []string{"Assembler"}

	baseLines := []string{fmt.Sprintf("Assembler module %s", module), "Operations:"}
	baseLines = append(baseLines, prependDash(operations)...)

	baseDoc := Doc{
		ID:           fmt.Sprintf("%s:asm", module),
		Program:      filepath.Base(path),
		SourcePath:   path,
		Chunk:        -60,
		Type:         "asm_module",
		Content:      strings.Join(nonEmpty(baseLines), "\n"),
		Summary:      fmt.Sprintf("Assembler module %s with %d operations", module, len(operations)),
		Technologies: technologies,
	}

	flowLines := []string{fmt.Sprintf("Assembler flow for %s", module)}
	flowLines = append(flowLines, prependDash(flowSteps)...)

	workingLines := []string{fmt.Sprintf("Working notes for %s", module)}
	if regs := uniqueStringsFromSlice(baseRegisters); len(regs) > 0 {
		workingLines = append(workingLines, fmt.Sprintf("- Base registers established: %s", strings.Join(regs, ", ")))
	}
	if len(registerUsage) > 0 {
		var entries []string
		for reg, contexts := range registerUsage {
			entries = append(entries, fmt.Sprintf("%s (%s)", reg, strings.Join(uniqueStringsFromSlice(contexts), ", ")))
		}
		sort.Strings(entries)
		workingLines = append(workingLines, fmt.Sprintf("- Register usage: %s", strings.Join(entries, "; ")))
	}
	if len(storageDefs) > 0 {
		uniqueStorage := uniqueStringsFromSlice(storageDefs)
		if len(uniqueStorage) > 0 {
			workingLines = append(workingLines, "- Storage definitions:")
			for _, def := range uniqueStorage {
				workingLines = append(workingLines, "  - "+def)
			}
		}
	}
	if len(working) > 0 {
		workingLines = append(workingLines, prependDash(working)...)
	}
	if len(workingLines) == 1 {
		workingLines = append(workingLines, "- Review register setup and storage definitions")
	}

	businessLines := []string{fmt.Sprintf("Business rules for %s", module)}
	if len(macros) > 0 {
		businessLines = append(businessLines, fmt.Sprintf("- Macro invocations: %s", strings.Join(macros, "; ")))
	}
	if len(business) > 0 {
		businessLines = append(businessLines, prependDash(business)...)
	} else {
		businessLines = append(businessLines, "- Critical module boundaries defined by assembler control statements")
	}

	docs := []Doc{baseDoc}
	if flow := strings.Join(nonEmpty(flowLines), "\n"); flow != "" {
		docs = append(docs, Doc{
			ID:           fmt.Sprintf("%s:asm:flow", module),
			Program:      filepath.Base(path),
			SourcePath:   path,
			Chunk:        -61,
			Type:         "flow",
			Content:      flow,
			Summary:      fmt.Sprintf("Assembler flow for %s", module),
			Technologies: technologies,
		})
	}
	if working := strings.Join(nonEmpty(workingLines), "\n"); working != "" {
		docs = append(docs, Doc{
			ID:           fmt.Sprintf("%s:asm:working", module),
			Program:      filepath.Base(path),
			SourcePath:   path,
			Chunk:        -62,
			Type:         "working_notes",
			Content:      working,
			Summary:      fmt.Sprintf("Working notes for %s", module),
			Technologies: technologies,
		})
	}
	if business := strings.Join(nonEmpty(businessLines), "\n"); business != "" {
		docs = append(docs, Doc{
			ID:           fmt.Sprintf("%s:asm:business", module),
			Program:      filepath.Base(path),
			SourcePath:   path,
			Chunk:        -63,
			Type:         "business_rules",
			Content:      business,
			Summary:      fmt.Sprintf("Business rules for %s", module),
			Technologies: technologies,
		})
	}

	return docs, nil
}

func normalizeAssemblerRegister(token string) string {
	cleaned := strings.TrimSpace(token)
	cleaned = strings.Trim(cleaned, "() ")
	if cleaned == "" {
		return ""
	}
	upper := strings.ToUpper(cleaned)
	if strings.HasPrefix(upper, "R") {
		upper = strings.TrimPrefix(upper, "R")
	}
	upper = strings.TrimSpace(upper)
	if upper == "" {
		return ""
	}
	if _, err := strconv.Atoi(upper); err == nil {
		return "R" + upper
	}
	return strings.ToUpper(cleaned)
}
