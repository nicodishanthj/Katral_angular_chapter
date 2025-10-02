// File path: internal/common/process/process.go
package process

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"io"
	"log/slog"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
	"time"

	"github.com/nicodishanthj/Katral_phase1/internal/common"
)

// ServiceConfig describes an external process that should be supervised.
type ServiceConfig struct {
	Name          string
	Command       string
	Args          []string
	Env           []string
	WorkDir       string
	ReadyURL      string
	ReadyTimeout  time.Duration
	ReadyInterval time.Duration
	StopTimeout   time.Duration
	Logger        *slog.Logger
}

// ManagedService tracks the lifecycle of a launched external process.
type ManagedService struct {
	cfg ServiceConfig
	cmd *exec.Cmd

	done    chan struct{}
	waitErr error
	mu      sync.RWMutex
}

// Start launches the configured process and waits for the readiness probe.
func Start(ctx context.Context, cfg ServiceConfig) (*ManagedService, error) {
	if strings.TrimSpace(cfg.Command) == "" {
		return nil, errors.New("process: command required")
	}
	if ctx == nil {
		ctx = context.Background()
	}
	if cfg.Logger != nil {
		cfg.Logger.Info(
			"process: launching service",
			"service", cfg.Name,
			"command", cfg.Command,
			"args", strings.Join(cfg.Args, " "),
		)
	}
	cmd := exec.CommandContext(ctx, cfg.Command, cfg.Args...)
	if cfg.WorkDir != "" {
		cmd.Dir = cfg.WorkDir
	}
	stdoutPipe, err := cmd.StdoutPipe()
	if err != nil {
		return nil, fmt.Errorf("process: stdout pipe %s: %w", cfg.Name, err)
	}
	stderrPipe, err := cmd.StderrPipe()
	if err != nil {
		stdoutPipe.Close()
		return nil, fmt.Errorf("process: stderr pipe %s: %w", cfg.Name, err)
	}
	if len(cfg.Env) > 0 {
		cmd.Env = append(os.Environ(), cfg.Env...)
	}

	if err := cmd.Start(); err != nil {
		stdoutPipe.Close()
		stderrPipe.Close()
		return nil, fmt.Errorf("process: start %s: %w", cfg.Name, err)
	}

	logger := cfg.Logger
	if logger == nil {
		logger = common.Logger()
	}
	componentName := strings.TrimSpace(cfg.Name)
	if componentName == "" {
		componentName = filepath.Base(strings.TrimSpace(cfg.Command))
	}
	if componentName == "" {
		componentName = "process"
	}
	componentKey := "service/" + strings.ReplaceAll(strings.ToLower(componentName), " ", "_")
	baseAttrs := []slog.Attr{
		slog.String("component", componentKey),
		slog.String("service", componentName),
	}

	streamCtx, cancelStreams := context.WithCancel(ctx)
	var streamWG sync.WaitGroup
	startForward := func(pipe io.ReadCloser, stream string) {
		if pipe == nil {
			return
		}
		streamWG.Add(1)
		go func() {
			defer streamWG.Done()
			var once sync.Once
			closePipe := func() { once.Do(func() { pipe.Close() }) }
			defer closePipe()
			done := make(chan struct{})
			go func() {
				select {
				case <-streamCtx.Done():
					closePipe()
				case <-done:
				}
			}()

			scanner := bufio.NewScanner(pipe)
			scanner.Buffer(make([]byte, 0, 64*1024), 1024*1024)
			attrs := append([]slog.Attr(nil), baseAttrs...)
			attrs = append(attrs, slog.String("stream", stream))
			level := slog.LevelInfo
			if stream == "stderr" {
				level = slog.LevelWarn
			}
			for scanner.Scan() {
				line := scanner.Text()
				logger.LogAttrs(streamCtx, level, line, attrs...)
			}
			close(done)
			if err := scanner.Err(); err != nil && streamCtx.Err() == nil && !errors.Is(err, os.ErrClosed) {
				errorAttrs := append([]slog.Attr(nil), attrs...)
				errorAttrs = append(errorAttrs, slog.Any("error", err))
				logger.LogAttrs(streamCtx, slog.LevelWarn, "process log stream error", errorAttrs...)
			}
		}()
	}

	startForward(stdoutPipe, "stdout")
	startForward(stderrPipe, "stderr")

	svc := &ManagedService{cfg: cfg, cmd: cmd, done: make(chan struct{})}
	go func() {
		err := cmd.Wait()
		cancelStreams()
		streamWG.Wait()
		svc.mu.Lock()
		svc.waitErr = err
		svc.mu.Unlock()
		close(svc.done)
	}()

	if err := waitForReady(ctx, svc); err != nil {
		svc.Stop(context.Background())
		return nil, err
	}
	if cfg.Logger != nil {
		cfg.Logger.Info("process: service ready", "service", cfg.Name, "url", cfg.ReadyURL)
	}
	return svc, nil
}

// Stop attempts a graceful shutdown followed by a forced kill if needed.
func (s *ManagedService) Stop(ctx context.Context) error {
	if s == nil {
		return nil
	}
	if ctx == nil {
		ctx = context.Background()
	}
	if s.cfg.Logger != nil {
		s.cfg.Logger.Info("process: stopping service", "service", s.cfg.Name)
	}
	if s.cmd != nil && s.cmd.Process != nil {
		if err := s.cmd.Process.Signal(os.Interrupt); err != nil && !errors.Is(err, os.ErrProcessDone) {
			if s.cfg.Logger != nil {
				s.cfg.Logger.Warn("process: interrupt failed", "service", s.cfg.Name, "error", err)
			}
		}
	}
	stopTimeout := s.cfg.StopTimeout
	if stopTimeout <= 0 {
		stopTimeout = 5 * time.Second
	}
	timer := time.NewTimer(stopTimeout)
	defer timer.Stop()

	select {
	case <-s.done:
		return s.normalizeWaitErr()
	case <-timer.C:
		if s.cfg.Logger != nil {
			s.cfg.Logger.Warn("process: forcing service kill", "service", s.cfg.Name)
		}
		if s.cmd != nil && s.cmd.Process != nil {
			if err := s.cmd.Process.Kill(); err != nil && !errors.Is(err, os.ErrProcessDone) {
				if s.cfg.Logger != nil {
					s.cfg.Logger.Error("process: kill failed", "service", s.cfg.Name, "error", err)
				}
				return err
			}
		}
		<-s.done
		return s.normalizeWaitErr()
	case <-ctx.Done():
		return ctx.Err()
	}
}

func waitForReady(ctx context.Context, svc *ManagedService) error {
	cfg := svc.cfg
	if strings.TrimSpace(cfg.ReadyURL) == "" {
		return nil
	}
	readyTimeout := cfg.ReadyTimeout
	if readyTimeout <= 0 {
		readyTimeout = 30 * time.Second
	}
	interval := cfg.ReadyInterval
	if interval <= 0 {
		interval = 500 * time.Millisecond
	}

	client := &http.Client{Timeout: 2 * time.Second}
	readyCtx, cancel := context.WithTimeout(ctx, readyTimeout)
	defer cancel()

	ticker := time.NewTicker(interval)
	defer ticker.Stop()

	var lastErr error
	for {
		select {
		case <-readyCtx.Done():
			if lastErr != nil {
				return fmt.Errorf("process: waiting for %s ready timed out after %s: last error: %w", cfg.Name, readyTimeout, lastErr)
			}
			return fmt.Errorf("process: waiting for %s ready timed out after %s: %w", cfg.Name, readyTimeout, readyCtx.Err())
		case <-svc.done:
			return fmt.Errorf("process: %s exited before reporting ready: %w", cfg.Name, svc.waitError())
		case <-ticker.C:
			req, err := http.NewRequestWithContext(readyCtx, http.MethodGet, cfg.ReadyURL, nil)
			if err != nil {
				return fmt.Errorf("process: build readiness request for %s: %w", cfg.Name, err)
			}
			resp, err := client.Do(req)
			if err != nil {
				lastErr = err
				continue
			}
			io.Copy(io.Discard, resp.Body)
			resp.Body.Close()
			if resp.StatusCode >= http.StatusOK && resp.StatusCode < http.StatusInternalServerError {
				return nil
			}
			lastErr = fmt.Errorf("unexpected status %d", resp.StatusCode)
		}
	}
}

func (s *ManagedService) waitError() error {
	s.mu.RLock()
	defer s.mu.RUnlock()
	return s.waitErr
}

func (s *ManagedService) normalizeWaitErr() error {
	err := s.waitError()
	if err == nil {
		return nil
	}
	var exitErr *exec.ExitError
	if errors.As(err, &exitErr) {
		if exitErr.Exited() {
			// Treat SIGINT/SIGKILL exits as graceful for shutdown scenarios.
			return nil
		}
	}
	return err
}

// BinaryPath resolves an executable path using the system PATH.
func BinaryPath(name string) (string, error) {
	if strings.TrimSpace(name) == "" {
		return "", errors.New("process: binary name required")
	}
	path, err := exec.LookPath(name)
	if err != nil {
		return "", fmt.Errorf("process: locate %s: %w", name, err)
	}
	return filepath.Clean(path), nil
}
