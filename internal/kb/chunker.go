// File path: internal/kb/chunker.go
package kb

import (
	"fmt"
	"strings"
)

// ChunkLines splits the provided lines into fixed-size chunks suitable for indexing.
func ChunkLines(program, source string, lines []string, size int) []Doc {
	if size <= 0 {
		size = 40
	}
	var docs []Doc
	var buffer []string
	chunkIndex := 0
	flush := func() {
		if len(buffer) == 0 {
			return
		}
		docs = append(docs, Doc{
			ID:         buildDocID(program, chunkIndex),
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

func buildDocID(program string, chunk int) string {
	program = strings.TrimSpace(program)
	if program == "" {
		program = "unknown"
	}
	return fmt.Sprintf("%s:%d", program, chunk)
}
