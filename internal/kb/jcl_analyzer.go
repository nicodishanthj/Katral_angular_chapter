// File path: internal/kb/jcl_analyzer.go
package kb

import (
	"context"
	"fmt"
	"path/filepath"
	"regexp"
	"strings"
)

type jclAnalyzer struct{}

var (
	jclJobRe  = regexp.MustCompile(`(?i)^//([A-Z0-9$#@]+)\s+JOB`)
	jclStepRe = regexp.MustCompile(`(?i)^//([A-Z0-9$#@]+)\s+EXEC\s+([^,\s]+)`)
	jclDDRe   = regexp.MustCompile(`(?i)^//([A-Z0-9$#@]+)\s+DD\s+(.*)`)
)

func (j *jclAnalyzer) Name() string { return "jcl" }

func (j *jclAnalyzer) Match(path string, data []byte) bool {
	lower := strings.ToLower(path)
	if strings.HasSuffix(lower, ".jcl") || strings.Contains(lower, ".job") {
		return true
	}
	upper := strings.ToUpper(string(data))
	return strings.Contains(upper, "//") && strings.Contains(upper, " JOB ")
}

func (j *jclAnalyzer) Parse(ctx context.Context, path string, data []byte) ([]Doc, error) {
	lines := strings.Split(string(data), "\n")
	jobName := ""
	var steps []string
	var datasets []string
	var comments []string
	var conditions []string
	ddSeen := make(map[string]struct{})
	ddOrder := make([]string, 0)
	ddDetail := make(map[string]string)
	var currentDD string
	technologies := []string{"JCL"}

	for _, raw := range lines {
		trimmed := strings.TrimRight(raw, " \t\r")
		upper := strings.ToUpper(strings.TrimSpace(trimmed))
		if strings.HasPrefix(strings.TrimSpace(trimmed), "//*") {
			comment := strings.TrimSpace(strings.TrimPrefix(strings.TrimSpace(trimmed), "//*"))
			if comment != "" {
				comments = append(comments, comment)
			}
			continue
		}
		if strings.HasPrefix(strings.TrimSpace(trimmed), "/*") {
			comment := strings.Trim(strings.TrimSpace(trimmed), "/*")
			if comment != "" {
				comments = append(comments, comment)
			}
			continue
		}
		if strings.HasPrefix(strings.TrimSpace(trimmed), "//") {
			if match := jclJobRe.FindStringSubmatch(upper); len(match) > 1 {
				jobName = strings.ToUpper(match[1])
			}
			if match := jclStepRe.FindStringSubmatch(upper); len(match) > 2 {
				stepName := strings.ToUpper(match[1])
				execTarget := strings.ToUpper(strings.Trim(match[2], "'\""))
				if execTarget == "" {
					execTarget = "EXEC"
				}
				steps = append(steps, fmt.Sprintf("%s executes %s", stepName, execTarget))
				currentDD = ""
			} else if match := jclDDRe.FindStringSubmatch(trimmed); len(match) > 2 {
				ddName := strings.ToUpper(strings.TrimSpace(match[1]))
				detail := strings.TrimSpace(match[2])
				if ddName != "" {
					if _, ok := ddSeen[ddName]; !ok {
						ddSeen[ddName] = struct{}{}
						ddOrder = append(ddOrder, ddName)
					}
					if existing := ddDetail[ddName]; existing != "" {
						detail = existing + " " + detail
					}
					ddDetail[ddName] = strings.TrimSpace(detail)
					currentDD = ddName
				}
			} else {
				currentDD = ""
			}
			if strings.Contains(upper, "COND=") || strings.Contains(upper, "IF(") {
				conditions = append(conditions, strings.TrimSpace(trimmed))
			}
			continue
		}
		if currentDD != "" {
			addition := strings.TrimSpace(trimmed)
			if addition != "" {
				if existing := ddDetail[currentDD]; existing != "" {
					ddDetail[currentDD] = existing + " " + addition
				} else {
					ddDetail[currentDD] = addition
				}
			}
			if strings.Contains(upper, "COND=") || strings.Contains(upper, "IF(") {
				conditions = append(conditions, strings.TrimSpace(trimmed))
			}
		}
	}

	if jobName == "" {
		jobName = strings.ToUpper(strings.TrimSuffix(filepath.Base(path), filepath.Ext(path)))
	}

	for _, name := range ddOrder {
		detail := ddDetail[name]
		if detail == "" {
			datasets = append(datasets, name)
			continue
		}
		datasets = append(datasets, fmt.Sprintf("%s -> %s", name, detail))
	}

	baseLines := []string{fmt.Sprintf("JCL job %s", jobName)}
	if len(steps) > 0 {
		baseLines = append(baseLines, "Steps:")
		baseLines = append(baseLines, prependDash(steps)...)
	}
	if len(datasets) > 0 {
		baseLines = append(baseLines, "Datasets:")
		baseLines = append(baseLines, prependDash(datasets)...)
	}
	baseDoc := Doc{
		ID:           fmt.Sprintf("%s:jcl", jobName),
		Program:      jobName,
		SourcePath:   path,
		Chunk:        -8,
		Type:         "jcl_job",
		Content:      strings.Join(nonEmpty(baseLines), "\n"),
		Summary:      fmt.Sprintf("JCL job %s with %d steps", jobName, len(steps)),
		Technologies: technologies,
		Extra: map[string]string{
			"steps":    strings.Join(steps, "; "),
			"datasets": strings.Join(datasets, "; "),
		},
	}

	flowLines := []string{fmt.Sprintf("Execution flow for %s", jobName)}
	if len(steps) > 0 {
		flowLines = append(flowLines, "Step order:")
		flowLines = append(flowLines, prependDash(steps)...)
	}
	if len(datasets) > 0 {
		flowLines = append(flowLines, "Referenced DD statements:")
		flowLines = append(flowLines, prependDash(datasets)...)
	}

	workingLines := []string{fmt.Sprintf("Working notes for %s", jobName)}
	if len(datasets) > 0 {
		workingLines = append(workingLines, "Dataset allocations:")
		workingLines = append(workingLines, prependDash(datasets)...)
	}
	if len(conditions) > 0 {
		workingLines = append(workingLines, "Condition handling:")
		workingLines = append(workingLines, prependDash(conditions)...)
	}
	if len(comments) > 0 {
		workingLines = append(workingLines, "Operator comments:")
		workingLines = append(workingLines, prependDash(comments)...)
	}

	businessLines := []string{fmt.Sprintf("Business controls in %s", jobName)}
	if len(conditions) > 0 {
		businessLines = append(businessLines, "Conditional execution rules:")
		businessLines = append(businessLines, prependDash(conditions)...)
	}
	if len(comments) > 0 {
		businessLines = append(businessLines, "Runbook notes:")
		businessLines = append(businessLines, prependDash(comments)...)
	}

	docs := []Doc{baseDoc}
	if flow := strings.Join(nonEmpty(flowLines), "\n"); flow != "" {
		docs = append(docs, Doc{
			ID:           fmt.Sprintf("%s:jcl:flow", jobName),
			Program:      jobName,
			SourcePath:   path,
			Chunk:        -9,
			Type:         "flow",
			Content:      flow,
			Summary:      fmt.Sprintf("JCL flow for %s", jobName),
			Technologies: technologies,
		})
	}
	if working := strings.Join(nonEmpty(workingLines), "\n"); working != "" {
		docs = append(docs, Doc{
			ID:           fmt.Sprintf("%s:jcl:working", jobName),
			Program:      jobName,
			SourcePath:   path,
			Chunk:        -10,
			Type:         "working_notes",
			Content:      working,
			Summary:      fmt.Sprintf("Working notes for %s", jobName),
			Technologies: technologies,
		})
	}
	if business := strings.Join(nonEmpty(businessLines), "\n"); business != "" {
		docs = append(docs, Doc{
			ID:           fmt.Sprintf("%s:jcl:business", jobName),
			Program:      jobName,
			SourcePath:   path,
			Chunk:        -11,
			Type:         "business_rules",
			Content:      business,
			Summary:      fmt.Sprintf("Business rules for %s", jobName),
			Technologies: technologies,
		})
	}

	return docs, nil
}
