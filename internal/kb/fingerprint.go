// File path: internal/kb/fingerprint.go
package kb

import (
	"crypto/sha256"
	"encoding/hex"
	"sort"
	"strings"
)

// ComputeFingerprint derives a stable hash for the document based on its
// meaningful content. The fingerprint intentionally ignores generated fields so
// repeated ingests of unchanged source artifacts can be detected.
func ComputeFingerprint(doc Doc) string {
	hasher := sha256.New()
	write := func(parts ...string) {
		for _, part := range parts {
			if part == "" {
				continue
			}
			_, _ = hasher.Write([]byte(part))
			_, _ = hasher.Write([]byte{0})
		}
	}

	write(doc.ID, doc.Type, doc.Program, doc.SourcePath)
	// Prefer structured content when available; otherwise fall back to the
	// summary so metadata-only docs can still be fingerprinted.
	if strings.TrimSpace(doc.Content) != "" {
		write(doc.Content)
	} else if strings.TrimSpace(doc.Summary) != "" {
		write(doc.Summary)
	}

	writeSlice := func(values []string) {
		if len(values) == 0 {
			return
		}
		cleaned := make([]string, 0, len(values))
		for _, v := range values {
			trimmed := strings.TrimSpace(v)
			if trimmed != "" {
				cleaned = append(cleaned, trimmed)
			}
		}
		if len(cleaned) == 0 {
			return
		}
		sort.Strings(cleaned)
		write(strings.Join(cleaned, ";"))
	}

	writeSlice(doc.Technologies)
	writeSlice(doc.Inputs)
	writeSlice(doc.Outputs)
	writeSlice(doc.Calls)
	writeSlice(doc.Paragraphs)
	writeSlice(doc.Logic)

	if len(doc.Extra) > 0 {
		keys := make([]string, 0, len(doc.Extra))
		for k := range doc.Extra {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		for _, k := range keys {
			write(k, doc.Extra[k])
		}
	}

	sum := hasher.Sum(nil)
	return hex.EncodeToString(sum)
}
