// File path: internal/kb/analyzer_utils.go
package kb

import (
	"crypto/sha1"
	"fmt"
	"path/filepath"
	"sort"
	"strings"
	"unicode"
)

func pathToIDFragment(path string) string {
	normalized := filepath.ToSlash(path)
	normalized = strings.Trim(normalized, "./")
	if normalized == "" {
		normalized = filepath.ToSlash(path)
	}

	var b strings.Builder
	prevDash := false
	for _, r := range normalized {
		switch {
		case unicode.IsLetter(r) || unicode.IsDigit(r):
			b.WriteRune(unicode.ToLower(r))
			prevDash = false
		case r == '/' || r == '-' || r == '_' || r == '.':
			if !prevDash {
				b.WriteRune('-')
				prevDash = true
			}
		default:
			if !prevDash {
				b.WriteRune('-')
				prevDash = true
			}
		}
	}

	result := strings.Trim(b.String(), "-")
	if result == "" {
		sum := sha1.Sum([]byte(normalized))
		result = fmt.Sprintf("id-%x", sum[:4])
	}
	return result
}

func prependDash(values []string) []string {
	var out []string
	for _, v := range values {
		trimmed := strings.TrimSpace(v)
		if trimmed == "" {
			continue
		}
		if strings.HasPrefix(trimmed, "-") {
			out = append(out, trimmed)
		} else {
			out = append(out, "- "+trimmed)
		}
	}
	return out
}

func nonEmpty(values []string) []string {
	var out []string
	for _, v := range values {
		trimmed := strings.TrimSpace(v)
		if trimmed == "" {
			continue
		}
		out = append(out, trimmed)
	}
	return out
}

func uniqueStringsFromSlice(values []string) []string {
	seen := make(map[string]struct{})
	var out []string
	for _, v := range values {
		if _, ok := seen[v]; ok {
			continue
		}
		seen[v] = struct{}{}
		out = append(out, v)
	}
	sort.Strings(out)
	return out
}
