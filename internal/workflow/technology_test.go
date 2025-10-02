// File path: internal/workflow/technology_test.go
package workflow

import (
	"context"
	"os"
	"path/filepath"
	"testing"
)

func TestDetectTechnologies(t *testing.T) {
	mgr := &Manager{}
	tmp := t.TempDir()
	repo := filepath.Join(tmp, "repo")
	if err := os.Mkdir(repo, 0o755); err != nil {
		t.Fatalf("mkdir repo: %v", err)
	}
	cobolPath := filepath.Join(repo, "sample.cbl")
	cobol := "IDENTIFICATION DIVISION.\nPROGRAM-ID. SAMPLE.\nDATA DIVISION.\nPROCEDURE DIVISION.\n    DISPLAY 'HELLO'.\n"
	if err := os.WriteFile(cobolPath, []byte(cobol), 0o644); err != nil {
		t.Fatalf("write cobol: %v", err)
	}
	jclPath := filepath.Join(repo, "job.jcl")
	jcl := "//JOBNAME JOB CLASS=A,MSGCLASS=X\n//STEP1 EXEC PGM=IEFBR14\n//DD1 DD DISP=SHR,DSN=MY.DATA.SET\n"
	if err := os.WriteFile(jclPath, []byte(jcl), 0o644); err != nil {
		t.Fatalf("write jcl: %v", err)
	}

	techs, err := mgr.DetectTechnologies(context.Background(), repo)
	if err != nil {
		t.Fatalf("DetectTechnologies returned error: %v", err)
	}
	if len(techs) == 0 {
		t.Fatalf("expected technologies to be detected")
	}
	want := map[string]bool{"COBOL": true, "JCL": true}
	for _, tech := range techs {
		delete(want, tech)
	}
	if len(want) != 0 {
		t.Fatalf("missing expected technologies: %v", want)
	}
}

func TestDetectTechnologiesRepoValidation(t *testing.T) {
	mgr := &Manager{}
	if _, err := mgr.DetectTechnologies(context.Background(), ""); err == nil {
		t.Fatalf("expected error for empty repo")
	}
	if _, err := mgr.DetectTechnologies(context.Background(), "/path/does/not/exist"); err == nil {
		t.Fatalf("expected error for missing repo")
	}
}
