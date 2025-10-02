// File path: internal/kb/cobol/parser_test.go
package cobol

import (
	"encoding/json"
	"testing"
)

func TestExtractProcedureLogic(t *testing.T) {
	src := []byte(`IDENTIFICATION DIVISION.
PROGRAM-ID. SAMPLE.
PROCEDURE DIVISION.
MAIN-PARA.
    IF CUSTOMER-STATUS = 'A'
        PERFORM PROCESS-A
    ELSE
        PERFORM PROCESS-B
    END-IF
    MOVE INPUT-AMT TO OUTPUT-AMT
    READ CUSTOMER-FILE
    EVALUATE ORDER-TYPE
        WHEN "ONLINE"
            MOVE SOURCE-FIELD TO TARGET-FIELD
        WHEN OTHER
            WRITE ERROR-LOG
    END-EVALUATE
PROCESS-A.
    PERFORM DETAIL-LOOP UNTIL DONE = 'Y'
DETAIL-LOOP.
    WRITE REPORT-REC.
`)

	parser := NewParser()
	docs, err := parser.Parse(nil, "sample.cbl", src)
	if err != nil {
		t.Fatalf("parse failed: %v", err)
	}
	if len(docs) == 0 {
		t.Fatalf("expected documents")
	}
	meta := docs[0]
	if meta.Type != "metadata" {
		t.Fatalf("expected metadata doc, got %s", meta.Type)
	}
	if len(meta.LogicTree) == 0 {
		t.Fatalf("expected logic tree")
	}
	if len(meta.LogicTree) < 4 {
		t.Fatalf("expected multiple top-level steps, got %d", len(meta.LogicTree))
	}

	ifStep := meta.LogicTree[0]
	if ifStep.Type != "if" {
		t.Fatalf("expected first step to be if, got %s", ifStep.Type)
	}
	if ifStep.Condition != "CUSTOMER-STATUS = 'A'" {
		t.Fatalf("unexpected if condition: %s", ifStep.Condition)
	}
	if len(ifStep.Children) == 0 || ifStep.Children[0].Type != "perform" {
		t.Fatalf("expected perform child in if branch")
	}
	if len(ifStep.Children[0].Targets) == 0 || ifStep.Children[0].Targets[0] != "PROCESS-A" {
		t.Fatalf("expected perform of PROCESS-A")
	}
	if len(ifStep.Else) == 0 || ifStep.Else[0].Type != "perform" {
		t.Fatalf("expected perform in else branch")
	}
	if len(ifStep.Else[0].Targets) == 0 || ifStep.Else[0].Targets[0] != "PROCESS-B" {
		t.Fatalf("expected perform of PROCESS-B")
	}

	moveFound := false
	readFound := false
	for _, step := range meta.LogicTree {
		if step.Type == "move" && step.Data["to"] == "OUTPUT-AMT" {
			moveFound = true
		}
		if step.Type == "read" && len(step.Targets) > 0 && step.Targets[0] == "CUSTOMER-FILE" {
			readFound = true
		}
	}
	if !moveFound {
		t.Fatalf("expected MOVE statement detection")
	}
	if !readFound {
		t.Fatalf("expected READ statement detection")
	}

	var paragraphMap map[string][]string
	if raw := meta.Extra["paragraph_logic"]; raw == "" {
		t.Fatalf("expected paragraph logic metadata")
	} else if err := json.Unmarshal([]byte(raw), &paragraphMap); err != nil {
		t.Fatalf("invalid paragraph metadata: %v", err)
	}
	if len(paragraphMap["MAIN-PARA"]) == 0 {
		t.Fatalf("expected MAIN-PARA entries in paragraph map")
	}
	if len(meta.Logic) == 0 {
		t.Fatalf("expected summary logic lines")
	}

	var evaluateFound bool
	for _, step := range meta.LogicTree {
		if step.Type == "evaluate" {
			evaluateFound = true
			if len(step.Children) < 2 {
				t.Fatalf("expected two WHEN branches")
			}
			foundCase := false
			for _, child := range step.Children {
				if child.Type == "when" && child.Condition == "\"ONLINE\"" {
					foundCase = true
					if len(child.Children) == 0 || child.Children[0].Type != "move" {
						t.Fatalf("expected MOVE under WHEN ONLINE")
					}
				}
			}
			if !foundCase {
				t.Fatalf("expected WHEN ONLINE branch")
			}
		}
	}
	if !evaluateFound {
		t.Fatalf("expected evaluate statement detection")
	}
}
