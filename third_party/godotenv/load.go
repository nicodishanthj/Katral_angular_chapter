// File path: third_party/godotenv/load.go
package godotenv

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

// Load reads the provided .env files (defaulting to ".env") and loads the
// contained key/value pairs into the process environment.
func Load(filenames ...string) error {
	if len(filenames) == 0 {
		filenames = []string{".env"}
	}

	var firstErr error
	for _, name := range filenames {
		if err := loadFile(name); err != nil {
			if firstErr == nil {
				firstErr = err
			}
			continue
		}
		return nil
	}

	if firstErr != nil {
		return firstErr
	}
	return fmt.Errorf("godotenv: no .env files specified")
}

func loadFile(name string) error {
	file, err := os.Open(name)
	if err != nil {
		return err
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	lineNo := 0
	for scanner.Scan() {
		lineNo++
		line := strings.TrimSpace(scanner.Text())
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}

		if strings.HasPrefix(line, "export ") {
			line = strings.TrimSpace(line[len("export "):])
		}

		key, value, ok := strings.Cut(line, "=")
		if !ok {
			return fmt.Errorf("godotenv: invalid line %d in %s", lineNo, name)
		}

		key = strings.TrimSpace(key)
		value = strings.TrimSpace(value)
		if key == "" {
			return fmt.Errorf("godotenv: invalid key on line %d in %s", lineNo, name)
		}

		parsed, err := parseValue(value)
		if err != nil {
			return fmt.Errorf("godotenv: %w", err)
		}

		if err := os.Setenv(key, parsed); err != nil {
			return err
		}
	}

	if err := scanner.Err(); err != nil {
		return err
	}

	return nil
}

func parseValue(value string) (string, error) {
	if value == "" {
		return "", nil
	}

	if len(value) >= 2 {
		if value[0] == value[len(value)-1] && (value[0] == '"' || value[0] == '\'') {
			inner := value[1 : len(value)-1]
			if value[0] == '"' {
				replacer := strings.NewReplacer(
					`\\`, `\`,
					`\n`, "\n",
					`\r`, "\r",
					`\t`, "\t",
					`\"`, `"`,
				)
				return replacer.Replace(inner), nil
			}
			return inner, nil
		}
	}

	return value, nil
}
