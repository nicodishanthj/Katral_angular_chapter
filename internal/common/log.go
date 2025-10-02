// File path: internal/common/log.go
package common

import (
	"context"
	"fmt"
	"log/slog"
	"os"
	"strings"
	"sync"
	"time"
)

const defaultLogHistory = 1000

var (
	logger     *slog.Logger
	loggerOnce sync.Once
	sink       = newLogSink(defaultLogHistory)
)

// LogEntry represents a captured log record emitted via the common logger.
type LogEntry struct {
	Time       time.Time              `json:"time"`
	Level      string                 `json:"level"`
	Message    string                 `json:"message"`
	Component  string                 `json:"component,omitempty"`
	Attributes map[string]interface{} `json:"attributes,omitempty"`
}

// Logger returns a singleton slog logger configured via LOG_LEVEL environment variable.
func Logger() *slog.Logger {
	loggerOnce.Do(func() {
		level := slog.LevelInfo
		if lvl := strings.ToLower(os.Getenv("LOG_LEVEL")); lvl != "" {
			switch lvl {
			case "debug":
				level = slog.LevelDebug
			case "warn":
				level = slog.LevelWarn
			case "error":
				level = slog.LevelError
			}
		}
		baseHandler := slog.NewTextHandler(os.Stdout, &slog.HandlerOptions{Level: level})
		handler := &capturingHandler{handler: baseHandler, sink: sink}
		logger = slog.New(handler)
	})
	return logger
}

// LogEntries returns a copy of the captured log entries.
func LogEntries() []LogEntry {
	if sink == nil {
		return nil
	}
	return sink.entries()
}

type capturingHandler struct {
	handler slog.Handler
	sink    *logSink
}

func (h *capturingHandler) Enabled(ctx context.Context, level slog.Level) bool {
	return h.handler.Enabled(ctx, level)
}

func (h *capturingHandler) Handle(ctx context.Context, record slog.Record) error {
	err := h.handler.Handle(ctx, record)
	if h.sink != nil {
		h.sink.capture(record)
	}
	return err
}

func (h *capturingHandler) WithAttrs(attrs []slog.Attr) slog.Handler {
	return &capturingHandler{handler: h.handler.WithAttrs(attrs), sink: h.sink}
}

func (h *capturingHandler) WithGroup(name string) slog.Handler {
	return &capturingHandler{handler: h.handler.WithGroup(name), sink: h.sink}
}

type logSink struct {
	mu      sync.RWMutex
	max     int
	history []LogEntry
}

func newLogSink(max int) *logSink {
	if max <= 0 {
		max = defaultLogHistory
	}
	return &logSink{max: max}
}

func (s *logSink) capture(record slog.Record) {
	entry := buildLogEntry(record)
	s.mu.Lock()
	defer s.mu.Unlock()
	s.history = append(s.history, entry)
	if len(s.history) > s.max {
		s.history = s.history[len(s.history)-s.max:]
	}
}

func (s *logSink) entries() []LogEntry {
	s.mu.RLock()
	defer s.mu.RUnlock()
	if len(s.history) == 0 {
		return nil
	}
	out := make([]LogEntry, len(s.history))
	copy(out, s.history)
	return out
}

func buildLogEntry(record slog.Record) LogEntry {
	rec := record.Clone()
	entry := LogEntry{
		Time:    rec.Time,
		Level:   strings.ToLower(rec.Level.String()),
		Message: rec.Message,
	}
	if entry.Time.IsZero() {
		entry.Time = time.Now()
	}
	if entry.Time.Location() != time.UTC {
		entry.Time = entry.Time.In(time.UTC)
	}

	var attrs map[string]interface{}
	rec.Attrs(func(a slog.Attr) bool {
		value := valueToAny(a.Value)
		if a.Key == "component" {
			if str, ok := value.(string); ok && str != "" {
				entry.Component = str
			} else if value != nil {
				entry.Component = strings.TrimSpace(valueString(value))
			}
			return true
		}
		if attrs == nil {
			attrs = make(map[string]interface{})
		}
		attrs[a.Key] = value
		return true
	})

	if entry.Component == "" {
		if idx := strings.Index(entry.Message, ":"); idx > 0 {
			component := strings.TrimSpace(entry.Message[:idx])
			if component != "" {
				entry.Component = component
			}
		}
	}

	if attrs != nil && len(attrs) > 0 {
		entry.Attributes = attrs
	}

	return entry
}

func valueToAny(v slog.Value) interface{} {
	switch v.Kind() {
	case slog.KindString:
		return v.String()
	case slog.KindBool:
		return v.Bool()
	case slog.KindInt64:
		return v.Int64()
	case slog.KindUint64:
		return v.Uint64()
	case slog.KindFloat64:
		return v.Float64()
	case slog.KindDuration:
		return v.Duration().String()
	case slog.KindTime:
		return v.Time().In(time.UTC)
	case slog.KindAny:
		return v.Any()
	default:
		return v.String()
	}
}

func valueString(v interface{}) string {
	switch val := v.(type) {
	case string:
		return val
	case fmt.Stringer:
		return val.String()
	default:
		return fmt.Sprint(val)
	}
}
