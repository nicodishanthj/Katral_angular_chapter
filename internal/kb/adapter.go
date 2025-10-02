// File path: internal/kb/adapter.go
package kb

import "strings"

// Text returns the textual content for vector embeddings.
func (d Doc) Text() string {
	base := d.Content
	if base == "" {
		base = d.Summary
	}
	if base == "" {
		if len(d.Technologies) > 0 {
			return "Technologies: " + strings.Join(d.Technologies, ", ")
		}
		return ""
	}
	if len(d.Technologies) == 0 {
		return base
	}
	return base + "\n\nTechnologies: " + strings.Join(d.Technologies, ", ")
}

// Label returns a human-friendly label for the document chunk.
func (d Doc) Label() string {
	if d.Chunk >= 0 {
		if len(d.Technologies) > 0 {
			return d.Program + " (" + strings.Join(d.Technologies, ", ") + ")"
		}
		return d.Program
	}
	if len(d.Technologies) > 0 {
		return d.Program + " metadata (" + strings.Join(d.Technologies, ", ") + ")"
	}
	return d.Program + " metadata"
}
