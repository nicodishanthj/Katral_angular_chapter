// File path: internal/kb/analyzers_test.go
package kb

import (
	"context"
	"strings"
	"testing"

	"github.com/nicodishanthj/Katral_phase1/internal/kb/cobol"
)

func TestCobolAnalyzerBuildsNarratives(t *testing.T) {
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

	analyzer := &cobolAnalyzer{parser: cobol.NewParser()}
	docs, err := analyzer.Parse(context.Background(), "sample.cbl", src)
	if err != nil {
		t.Fatalf("analyzer parse failed: %v", err)
	}

	var metadata Doc
	var flow Doc
	var business Doc
	for _, doc := range docs {
		switch doc.Type {
		case "metadata":
			metadata = doc
		case "flow":
			flow = doc
		case "business_rules":
			business = doc
		}
	}
	if metadata.Type == "" {
		t.Fatalf("metadata doc not found")
	}
	if len(metadata.LogicTree) == 0 {
		t.Fatalf("expected structured logic in metadata")
	}
	if flow.Type == "" {
		t.Fatalf("flow doc not created")
	}
	if !strings.Contains(flow.Content, "Control flow overview") {
		t.Fatalf("flow content missing overview: %s", flow.Content)
	}
	if !strings.Contains(flow.Content, "Perform PROCESS-A") {
		t.Fatalf("flow content missing perform narrative: %s", flow.Content)
	}
	if business.Type == "" {
		t.Fatalf("business rules doc not created")
	}
	if !strings.Contains(business.Content, "If CUSTOMER-STATUS = 'A' then") {
		t.Fatalf("business content missing branch narrative: %s", business.Content)
	}
	if !strings.Contains(business.Content, "Move INPUT-AMT to OUTPUT-AMT") {
		t.Fatalf("business content missing move narrative: %s", business.Content)
	}
}

func TestCICSAnalyzerCapturesOperands(t *testing.T) {
	src := []byte(`       EXEC CICS SEND MAP('ACCTMAP') MAPSET('ACCTSET') TRANSID('PAY1') END-EXEC.
       EXEC CICS LINK PROGRAM('PAYCALC') CHANNEL('PAYCHAN') COMMAREA(WS-COMMAREA) END-EXEC.`)

	analyzer := &cicsAnalyzer{}
	docs, err := analyzer.Parse(context.Background(), "payprog.cbl", src)
	if err != nil {
		t.Fatalf("cics analyzer failed: %v", err)
	}

	var base Doc
	var flow Doc
	var working Doc
	for _, doc := range docs {
		switch doc.Type {
		case "cics_flow":
			base = doc
		case "flow":
			if hasTechnology(doc, "CICS") {
				flow = doc
			}
		case "working_notes":
			if hasTechnology(doc, "CICS") {
				working = doc
			}
		}
	}
	if base.Type == "" {
		t.Fatalf("expected base CICS doc")
	}
	if !strings.Contains(base.Content, "MAP=ACCTMAP") {
		t.Fatalf("expected MAP operand in base content: %s", base.Content)
	}
	if flow.Type == "" || !strings.Contains(flow.Content, "Step 2: LINK") || !strings.Contains(flow.Content, "CHANNEL=PAYCHAN") {
		t.Fatalf("expected flow details with channel, got: %s", flow.Content)
	}
	if working.Type == "" {
		t.Fatalf("expected CICS working notes")
	}
	if !strings.Contains(working.Content, "Maps referenced: ACCTMAP") {
		t.Fatalf("working notes missing map reference: %s", working.Content)
	}
	if working.Extra == nil || working.Extra["channel"] != "PAYCHAN" {
		t.Fatalf("expected channel in working extras: %#v", working.Extra)
	}
}

func TestCICSAnalyzerGeneratesUniqueIDsPerPath(t *testing.T) {
	ctx := context.Background()
	src := []byte(`       EXEC CICS SEND MAP('MAP1') END-EXEC.`)

	analyzer := &cicsAnalyzer{}

	docsA, err := analyzer.Parse(ctx, "src/payprog.cbl", src)
	if err != nil {
		t.Fatalf("parse for first path failed: %v", err)
	}
	docsB, err := analyzer.Parse(ctx, "src/payprog.cpy", src)
	if err != nil {
		t.Fatalf("parse for second path failed: %v", err)
	}

	var baseA, baseB Doc
	for _, doc := range docsA {
		if doc.Type == "cics_flow" {
			baseA = doc
			break
		}
	}
	for _, doc := range docsB {
		if doc.Type == "cics_flow" {
			baseB = doc
			break
		}
	}

	if baseA.ID == "" || baseB.ID == "" {
		t.Fatalf("expected both parses to produce base docs: %#v %#v", baseA, baseB)
	}
	if baseA.ID == baseB.ID {
		t.Fatalf("expected unique IDs, got duplicate %q", baseA.ID)
	}
}

func TestMQAnalyzerExtractsMessagingContext(t *testing.T) {
	src := []byte(`MQOPEN QUEUE('ORDER.IN') OPTIONS
MQPUT QUEUE('ORDER.OUT') MQMD(MSG-OUT) CORRELID(ORDER-ID)
MQGET QUEUE('ORDER.IN') MQMD(MSG-IN) CORRELID(ORDER-ID) WAIT
CALL 'MQCLOSE' USING HCONN HOBJ
`)

	analyzer := &mqAnalyzer{}
	docs, err := analyzer.Parse(context.Background(), "queues.cbl", src)
	if err != nil {
		t.Fatalf("mq analyzer failed: %v", err)
	}

	var base Doc
	var flow Doc
	var working Doc
	var business Doc
	for _, doc := range docs {
		switch doc.Type {
		case "mq_flow":
			base = doc
		case "flow":
			if hasTechnology(doc, "MQ") {
				flow = doc
			}
		case "working_notes":
			if hasTechnology(doc, "MQ") {
				working = doc
			}
		case "business_rules":
			if hasTechnology(doc, "MQ") {
				business = doc
			}
		}
	}

	if base.Type == "" || !strings.Contains(base.Content, "ORDER.OUT") {
		t.Fatalf("expected MQ base summary with queues: %s", base.Content)
	}
	if base.Extra == nil || !strings.Contains(base.Extra["queues"], "ORDER.OUT") {
		t.Fatalf("expected queue summary in extras: %#v", base.Extra)
	}
	if flow.Type == "" || !strings.Contains(flow.Content, "MQPUT to ORDER.OUT") {
		t.Fatalf("expected flow with MQPUT detail: %s", flow.Content)
	}
	if working.Type == "" {
		t.Fatalf("expected MQ working notes")
	}
	if !strings.Contains(working.Content, "Producer calls") || !strings.Contains(working.Content, "Message descriptors observed") {
		t.Fatalf("working notes missing producer or descriptor summary: %s", working.Content)
	}
	if working.Extra == nil || working.Extra["correlation_ids"] != "ORDER-ID" {
		t.Fatalf("expected correlation id in extras: %#v", working.Extra)
	}
	if business.Type == "" || !strings.Contains(business.Content, "ORDER.IN") {
		t.Fatalf("expected MQ business notes mentioning queue consumption: %s", business.Content)
	}
}

func TestAsmAnalyzerCapturesOperandDetails(t *testing.T) {
	src := []byte(`ORDMOD   CSECT
ORDMOD   START 0
         USING *,15
         L     R3,=F'10'
         ST    R3,SAVEAREA+4
         CALL  PROCESS
SAVEAREA DS    18F
BUFFER   DC    CL10'ORDER'
* verify store
         END
`)

	analyzer := &asmAnalyzer{}
	docs, err := analyzer.Parse(context.Background(), "ordmod.asm", src)
	if err != nil {
		t.Fatalf("asm analyzer failed: %v", err)
	}

	var base Doc
	var flow Doc
	var working Doc
	var business Doc
	for _, doc := range docs {
		switch doc.Type {
		case "asm_module":
			base = doc
		case "flow":
			if hasTechnology(doc, "Assembler") {
				flow = doc
			}
		case "working_notes":
			if hasTechnology(doc, "Assembler") {
				working = doc
			}
		case "business_rules":
			if hasTechnology(doc, "Assembler") {
				business = doc
			}
		}
	}

	if base.Type == "" {
		t.Fatalf("expected assembler base doc")
	}
	if flow.Type == "" || !strings.Contains(flow.Content, "USING *,15") {
		t.Fatalf("expected flow with USING operands: %s", flow.Content)
	}
	if working.Type == "" || !strings.Contains(working.Content, "Base registers established: R15") || !strings.Contains(working.Content, "Register usage") {
		t.Fatalf("expected working notes with register insight: %s", working.Content)
	}
	if !strings.Contains(working.Content, "Storage definitions") || !strings.Contains(working.Content, "BUFFER DC CL10'ORDER'") || !strings.Contains(working.Content, "SAVEAREA DS 18F") {
		t.Fatalf("expected storage definitions in working notes: %s", working.Content)
	}
	if business.Type == "" || !strings.Contains(business.Content, "Macro invocations: CALL PROCESS") || !strings.Contains(business.Content, "verify store") {
		t.Fatalf("expected business notes with macros and comments: %s", business.Content)
	}
}
