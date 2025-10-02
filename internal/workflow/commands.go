// File path: internal/workflow/commands.go
package workflow

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"unicode"
)

type commandVariant struct {
	command string
	args    []string
}

func (m *Manager) executeGeneratorCommand(ctx context.Context, req Request) (string, error) {
	parts, err := parseCommandLine(req.GeneratorCommand)
	if err != nil {
		return "", err
	}
	if len(parts) == 0 {
		return "", fmt.Errorf("generator command parsed to empty input")
	}
	workDir := req.GeneratorDir
	if workDir == "" {
		workDir = req.SpringProject
	}
	if details := formatTargetConfigDetails(req.TargetConfig); details != "" {
		m.AppendLog("info", "Executing generator command for target %s", details)
	}
	output, err := runCommand(ctx, workDir, parts[0], parts[1:])
	if err != nil {
		return output, fmt.Errorf("generator command failed: %w", err)
	}
	return fmt.Sprintf("Generator command (%s) output:\n%s", strings.Join(parts, " "), strings.TrimSpace(output)), nil
}

func (m *Manager) verifySpringProject(ctx context.Context, req Request) error {
	project := req.SpringProject
	if project == "" {
		return fmt.Errorf("spring project path required for verification")
	}
	if details := formatTargetConfigDetails(req.TargetConfig); details != "" {
		m.AppendLog("info", "Verifying Spring project for target %s", details)
	}
	buildOutput, err := runProjectCommand(ctx, project, []commandVariant{{"./mvnw", []string{"clean", "package"}}, {"./gradlew", []string{"build"}}, {"go", []string{"build", "./..."}}})
	if err != nil {
		if buildOutput != "" {
			m.AppendLog("error", buildOutput)
		}
		return fmt.Errorf("spring build failed: %w", err)
	}
	if strings.TrimSpace(buildOutput) != "" {
		m.AppendLog("info", fmt.Sprintf("Spring build output:\n%s", strings.TrimSpace(buildOutput)))
	}
	testOutput, err := runProjectCommand(ctx, project, []commandVariant{{"./mvnw", []string{"test"}}, {"./gradlew", []string{"test"}}, {"go", []string{"test", "./..."}}})
	if err != nil {
		if testOutput != "" {
			m.AppendLog("error", testOutput)
		}
		return fmt.Errorf("spring tests failed: %w", err)
	}
	if strings.TrimSpace(testOutput) != "" {
		m.AppendLog("info", fmt.Sprintf("Spring test output:\n%s", strings.TrimSpace(testOutput)))
	}
	return nil
}

func runProjectCommand(ctx context.Context, repo string, variants []commandVariant) (string, error) {
	for _, variant := range variants {
		candidate := variant.command
		if strings.HasPrefix(candidate, "./") {
			path := filepath.Join(repo, candidate)
			if _, err := os.Stat(path); err == nil {
				return runCommand(ctx, repo, path, variant.args)
			}
		}
		if _, err := exec.LookPath(candidate); err == nil {
			return runCommand(ctx, repo, candidate, variant.args)
		}
	}
	return "", fmt.Errorf("no suitable command found")
}

func runCommand(ctx context.Context, dir, command string, args []string) (string, error) {
	cmd := exec.CommandContext(ctx, command, args...)
	if dir != "" {
		cmd.Dir = dir
	}
	var buf bytes.Buffer
	cmd.Stdout = &buf
	cmd.Stderr = &buf
	err := cmd.Run()
	return buf.String(), err
}

func parseCommandLine(input string) ([]string, error) {
	var args []string
	var current strings.Builder
	var quote rune
	escape := false
	for _, r := range input {
		switch {
		case escape:
			current.WriteRune(r)
			escape = false
		case r == '\\':
			escape = true
		case quote != 0:
			if r == quote {
				quote = 0
			} else {
				current.WriteRune(r)
			}
		case r == '\'' || r == '"':
			quote = r
		case unicode.IsSpace(r):
			if current.Len() > 0 {
				args = append(args, current.String())
				current.Reset()
			}
		default:
			current.WriteRune(r)
		}
	}
	if quote != 0 {
		return nil, fmt.Errorf("unterminated quote in command")
	}
	if escape {
		return nil, fmt.Errorf("dangling escape in command")
	}
	if current.Len() > 0 {
		args = append(args, current.String())
	}
	return args, nil
}
