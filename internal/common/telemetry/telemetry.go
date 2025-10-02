// File path: internal/common/telemetry/telemetry.go
package telemetry

import (
	"context"
	"expvar"
	"fmt"
	"os"
	"runtime"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/nicodishanthj/Katral_phase1/internal/common"
)

type spanKey struct{}

type span struct {
	name  string
	start time.Time
}

type MemoryLimitError struct {
	Component string
	Usage     uint64
	Limit     uint64
}

func (e MemoryLimitError) Error() string {
	return fmt.Sprintf("memory limit exceeded for %s: %d > %d", e.Component, e.Usage, e.Limit)
}

var (
	initOnce sync.Once

	vectorSearchTotal     *expvar.Int
	vectorSearchCacheHits *expvar.Int
	vectorSearchLatencyMS *expvar.Int

	graphQueryTotal     *expvar.Map
	graphQueryLatencyMS *expvar.Map

	ingestBatchTotal *expvar.Int
	ingestDocsTotal  *expvar.Int

	memoryLimitBytes uint64
	memoryLimitVar   *expvar.Int
	memoryUsageVar   *expvar.Int
)

func ensureInit() {
	initOnce.Do(func() {
		vectorSearchTotal = expvar.NewInt("katral_vector_search_total")
		vectorSearchCacheHits = expvar.NewInt("katral_vector_search_cache_hits")
		vectorSearchLatencyMS = expvar.NewInt("katral_vector_search_latency_ms")

		graphQueryTotal = expvar.NewMap("katral_graph_query_total")
		graphQueryLatencyMS = expvar.NewMap("katral_graph_query_latency_ms")

		ingestBatchTotal = expvar.NewInt("katral_ingest_batches_total")
		ingestDocsTotal = expvar.NewInt("katral_ingest_docs_total")

		memoryLimitVar = expvar.NewInt("katral_memory_limit_bytes")
		memoryUsageVar = expvar.NewInt("katral_memory_usage_bytes")

		memoryLimitBytes = loadMemoryLimit()
		memoryLimitVar.Set(int64(memoryLimitBytes))
	})
}

func loadMemoryLimit() uint64 {
	limit := strings.TrimSpace(os.Getenv("KATRAL_MEMORY_LIMIT_BYTES"))
	if limit != "" {
		if value, err := strconv.ParseUint(limit, 10, 64); err == nil {
			return value
		}
	}
	if limitMB := strings.TrimSpace(os.Getenv("KATRAL_MEMORY_LIMIT_MB")); limitMB != "" {
		if value, err := strconv.ParseUint(limitMB, 10, 64); err == nil {
			return value * 1024 * 1024
		}
	}
	return 0
}

func StartSpan(ctx context.Context, name string) (context.Context, func(attrs ...interface{})) {
	ensureInit()
	sp := &span{name: name, start: time.Now()}
	ctx = context.WithValue(ctx, spanKey{}, sp)
	logger := common.Logger()
	logger.Debug("trace: start", "span", name)
	return ctx, func(attrs ...interface{}) {
		if sp == nil {
			return
		}
		duration := time.Since(sp.start)
		logger.Debug("trace: end", append([]interface{}{"span", name, "dur", duration}, attrs...)...)
	}
}

func RecordVectorSearch(cacheHit bool, duration time.Duration) {
	ensureInit()
	vectorSearchTotal.Add(1)
	if cacheHit {
		vectorSearchCacheHits.Add(1)
	}
	if duration > 0 {
		vectorSearchLatencyMS.Add(duration.Milliseconds())
	}
}

func RecordGraphQuery(kind string, duration time.Duration) {
	ensureInit()
	key := strings.TrimSpace(strings.ToLower(kind))
	if key == "" {
		key = "unknown"
	}
	graphQueryTotal.Add(key, 1)
	if duration > 0 {
		graphQueryLatencyMS.Add(key, duration.Milliseconds())
	}
}

func RecordIngestBatch(kind string, docs int) {
	ensureInit()
	if docs <= 0 {
		return
	}
	key := strings.TrimSpace(strings.ToLower(kind))
	if key == "" {
		key = "generic"
	}
	ingestBatchTotal.Add(1)
	ingestDocsTotal.Add(int64(docs))
}

func CheckMemoryBudget(component string) error {
	ensureInit()
	if memoryLimitBytes == 0 {
		updateMemoryUsage()
		return nil
	}
	usage := updateMemoryUsage()
	if usage > memoryLimitBytes {
		err := MemoryLimitError{Component: component, Usage: usage, Limit: memoryLimitBytes}
		common.Logger().Warn("telemetry: memory guard tripped", "component", component, "usage", usage, "limit", memoryLimitBytes)
		return err
	}
	return nil
}

func updateMemoryUsage() uint64 {
	var stats runtime.MemStats
	runtime.ReadMemStats(&stats)
	usage := stats.Alloc
	memoryUsageVar.Set(int64(usage))
	return usage
}

func SpanDuration(ctx context.Context) time.Duration {
	sp, _ := ctx.Value(spanKey{}).(*span)
	if sp == nil {
		return 0
	}
	return time.Since(sp.start)
}
