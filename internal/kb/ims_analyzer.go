// File path: internal/kb/ims_analyzer.go
package kb

import (
	"context"
	"fmt"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
)

type imsAnalyzer struct{}

func (i *imsAnalyzer) Name() string { return "ims" }

func (i *imsAnalyzer) Match(path string, data []byte) bool {
	content := strings.ToUpper(string(data))
	if strings.Contains(content, "DBD ") || strings.Contains(content, "DBDNAME") {
		return true
	}
	lower := strings.ToLower(path)
	return strings.HasSuffix(lower, ".dbd")
}

func (i *imsAnalyzer) Parse(ctx context.Context, path string, data []byte) ([]Doc, error) {
	lines := strings.Split(string(data), "\n")
	var dbdName string
	var currentSeg string
	segmentOrder := make([]string, 0)
	segmentParents := make(map[string]string)
	fieldsBySeg := make(map[string][]string)
	var rules []string
	technologies := []string{"IMS"}

	segmentRe := regexp.MustCompile(`(?i)SEGM\s+NAME=([A-Z0-9-]+)(?:.*?PARENT=([A-Z0-9-]+))?`)
	fieldRe := regexp.MustCompile(`(?i)FIELD\s+NAME=([A-Z0-9-]+)(?:.*?LENGTH=([0-9]+))?`)

	for _, raw := range lines {
		line := strings.TrimSpace(raw)
		upper := strings.ToUpper(line)
		if dbdName == "" {
			if strings.HasPrefix(upper, "DBD") || strings.Contains(upper, "DBDNAME") {
				parts := strings.Split(upper, "=")
				if len(parts) > 1 {
					namePart := strings.TrimSpace(parts[len(parts)-1])
					namePart = strings.Trim(namePart, "',\"")
					if namePart != "" {
						dbdName = namePart
					}
				}
			}
		}
		if matches := segmentRe.FindStringSubmatch(upper); len(matches) > 1 {
			currentSeg = matches[1]
			if _, seen := fieldsBySeg[currentSeg]; !seen {
				segmentOrder = append(segmentOrder, currentSeg)
			}
			if len(matches) > 2 {
				parent := strings.Trim(matches[2], "',\"")
				if parent != "" {
					segmentParents[currentSeg] = parent
				}
			}
			continue
		}
		if matches := fieldRe.FindStringSubmatch(upper); len(matches) > 1 && currentSeg != "" {
			name := matches[1]
			length := ""
			if len(matches) > 2 {
				length = matches[2]
			}
			field := name
			if length != "" {
				field = fmt.Sprintf("%s (L=%s)", name, length)
			}
			fieldsBySeg[currentSeg] = append(fieldsBySeg[currentSeg], field)
			if strings.Contains(upper, "KEY=") {
				rules = append(rules, fmt.Sprintf("Segment %s defines key %s", currentSeg, name))
			}
		}
		if strings.Contains(upper, "PROCOPT=") {
			rules = append(rules, line)
		}
	}
	if dbdName == "" {
		dbdName = strings.ToUpper(strings.TrimSuffix(filepath.Base(path), filepath.Ext(path)))
	}

	var hierarchy []string
	for _, seg := range segmentOrder {
		parent := segmentParents[seg]
		if parent == "" {
			hierarchy = append(hierarchy, fmt.Sprintf("%s (root)", seg))
		} else {
			hierarchy = append(hierarchy, fmt.Sprintf("%s -> %s", parent, seg))
		}
	}

	var baseLines []string
	baseLines = append(baseLines, fmt.Sprintf("IMS DBD %s", dbdName))
	if len(hierarchy) > 0 {
		baseLines = append(baseLines, "Hierarchy:")
		baseLines = append(baseLines, prependDash(hierarchy)...)
	}
	for _, seg := range segmentOrder {
		fields := fieldsBySeg[seg]
		sort.Strings(fields)
		if len(fields) == 0 {
			continue
		}
		baseLines = append(baseLines, fmt.Sprintf("Segment %s fields:", seg))
		baseLines = append(baseLines, prependDash(fields)...)
	}

	baseDoc := Doc{
		ID:           fmt.Sprintf("%s:ims", dbdName),
		Program:      dbdName,
		SourcePath:   path,
		Chunk:        -12,
		Type:         "ims_schema",
		Content:      strings.Join(nonEmpty(baseLines), "\n"),
		Summary:      fmt.Sprintf("IMS database %s with %d segments", dbdName, len(segmentOrder)),
		Technologies: technologies,
		Extra: map[string]string{
			"segments": strings.Join(segmentOrder, ", "),
		},
	}

	flowLines := []string{fmt.Sprintf("IMS %s hierarchy", dbdName)}
	if len(hierarchy) > 0 {
		flowLines = append(flowLines, prependDash(hierarchy)...)
	}

	workingLines := []string{fmt.Sprintf("Working notes for %s", dbdName)}
	for _, seg := range segmentOrder {
		fields := fieldsBySeg[seg]
		if len(fields) == 0 {
			continue
		}
		workingLines = append(workingLines, fmt.Sprintf("Segment %s fields:", seg))
		workingLines = append(workingLines, prependDash(fields)...)
	}

	businessLines := []string{fmt.Sprintf("Business rules for %s", dbdName)}
	if len(rules) > 0 {
		businessLines = append(businessLines, prependDash(rules)...)
	} else if len(hierarchy) > 0 {
		businessLines = append(businessLines, prependDash(hierarchy)...)
	}

	docs := []Doc{baseDoc}
	if flow := strings.Join(nonEmpty(flowLines), "\n"); flow != "" {
		docs = append(docs, Doc{
			ID:           fmt.Sprintf("%s:ims:flow", dbdName),
			Program:      dbdName,
			SourcePath:   path,
			Chunk:        -13,
			Type:         "flow",
			Content:      flow,
			Summary:      fmt.Sprintf("IMS hierarchy for %s", dbdName),
			Technologies: technologies,
		})
	}
	if working := strings.Join(nonEmpty(workingLines), "\n"); working != "" {
		docs = append(docs, Doc{
			ID:           fmt.Sprintf("%s:ims:working", dbdName),
			Program:      dbdName,
			SourcePath:   path,
			Chunk:        -14,
			Type:         "working_notes",
			Content:      working,
			Summary:      fmt.Sprintf("IMS working notes for %s", dbdName),
			Technologies: technologies,
		})
	}
	if business := strings.Join(nonEmpty(businessLines), "\n"); business != "" {
		docs = append(docs, Doc{
			ID:           fmt.Sprintf("%s:ims:business", dbdName),
			Program:      dbdName,
			SourcePath:   path,
			Chunk:        -15,
			Type:         "business_rules",
			Content:      business,
			Summary:      fmt.Sprintf("IMS business rules for %s", dbdName),
			Technologies: technologies,
		})
	}

	return docs, nil
}
