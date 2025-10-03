package workflow

import (
	"os"
	"path/filepath"
	"testing"
)

func TestNormalizeRequestCleansMigrationMappings(t *testing.T) {
	tempDir := t.TempDir()
	// create dummy mainframe path requirement using temp dir
	mainframePath := filepath.Join(tempDir, "repo")
	if err := os.Mkdir(mainframePath, 0o755); err != nil {
		t.Fatalf("create repo dir: %v", err)
	}

	req := Request{
		ProjectID: "proj1",
		Mainframe: mainframePath,
		Stacks:    []string{" cobol "},
		TargetConfig: &TargetConfig{
			Framework: "React",
			Version:   "18",
		},
		MigrationConfig: &MigrationConfig{
			AngularSourceRoot: " ./src/app ",
			AngularVersion:    " 16 ",
			ReactTargetRoot:   " ./target ",
			ReactVersion:      "18",
			PatternMapping: map[string]string{
				" component ": " Component ",
				"":            "skip",
				"invalid":     " ",
			},
			ComponentLifecycleMapping: map[string]string{
				" OnInit ":  " useEffect ",
				"":          "",
				"OnDestroy": " ",
			},
			DirectiveConversionMapping: map[string]string{
				" *ngIf ": " conditional hook ",
			},
			ServiceContextMapping: map[string]string{
				" AuthService ": " useAuthContext ",
			},
			PipeConversionMapping: map[string]string{
				" date ": " formatDate ",
			},
			GuardRouteMapping: map[string]string{
				" AuthGuard ": " requireAuth ",
			},
		},
	}

	normalized, err := normalizeRequest(req)
	if err != nil {
		t.Fatalf("normalizeRequest returned error: %v", err)
	}

	mig := normalized.MigrationConfig
	if mig == nil {
		t.Fatalf("expected migration config to be preserved")
	}
	if got := mig.AngularSourceRoot; got != "./src/app" {
		t.Errorf("unexpected angular source root: %q", got)
	}
	if got := mig.PatternMapping; len(got) != 1 || got["component"] != "Component" {
		t.Errorf("unexpected pattern mapping: %#v", got)
	}
	if got := mig.ComponentLifecycleMapping; len(got) != 1 || got["OnInit"] != "useEffect" {
		t.Errorf("unexpected component lifecycle mapping: %#v", got)
	}
	if got := mig.DirectiveConversionMapping; len(got) != 1 || got["*ngIf"] != "conditional hook" {
		t.Errorf("unexpected directive conversion mapping: %#v", got)
	}
	if got := mig.ServiceContextMapping; len(got) != 1 || got["AuthService"] != "useAuthContext" {
		t.Errorf("unexpected service context mapping: %#v", got)
	}
	if got := mig.PipeConversionMapping; len(got) != 1 || got["date"] != "formatDate" {
		t.Errorf("unexpected pipe conversion mapping: %#v", got)
	}
	if got := mig.GuardRouteMapping; len(got) != 1 || got["AuthGuard"] != "requireAuth" {
		t.Errorf("unexpected guard route mapping: %#v", got)
	}
}

func TestCleanMappingReturnsNilWhenEmpty(t *testing.T) {
	if result := cleanMapping(nil); result != nil {
		t.Fatalf("expected nil for nil input, got %#v", result)
	}
	input := map[string]string{
		"":    "value",
		"key": "",
	}
	if result := cleanMapping(input); result != nil {
		t.Fatalf("expected nil for empty result, got %#v", result)
	}
}
