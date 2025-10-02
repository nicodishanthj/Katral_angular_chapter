// File path: internal/kb/indexer_test.go
package kb

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestIndexerParsesMainframeArtifacts(t *testing.T) {
	dir := t.TempDir()

	cobolSource := `IDENTIFICATION DIVISION.
PROGRAM-ID. SAMPLE.
ENVIRONMENT DIVISION.
DATA DIVISION.
FILE SECTION.
FD CUSTOMER-FILE.
01 CUSTOMER-REC.
   05 CUSTOMER-ID PIC X(10).
FD REPORT-OUT.
01 REPORT-REC.
   05 REPORT-ID PIC X(10).
PROCEDURE DIVISION.
MAIN-LOGIC.
    OPEN INPUT CUSTOMER-FILE
    OPEN OUTPUT REPORT-OUT
    READ CUSTOMER-FILE
    EXEC CICS SEND TEXT
    CALL 'PAYMENT'
    MOVE CUSTOMER-ID TO REPORT-ID
    EXEC SQL INSERT INTO CUSTOMER_LOG (ID) VALUES (:CUSTOMER-ID)
    WRITE REPORT-REC
    MQPUT
    CLOSE REPORT-OUT
    CLOSE CUSTOMER-FILE
    STOP RUN.
`
	ddl := `CREATE TABLE CUSTOMER_LOG (
    ID INT NOT NULL,
    DESCRIPTION VARCHAR(64),
    LAST_UPDATE TIMESTAMP NOT NULL DEFAULT CURRENT TIMESTAMP,
    PRIMARY KEY (ID)
);
`
	imsDBD := `DBD NAME=CUSTDB
DATASET DD1=IMS01
SEGM NAME=CUSTROOT PARENT=0
FIELD NAME=CUSTID,START=1,LENGTH=10
FIELD NAME=STATUS,START=11,LENGTH=2,KEY=YES
SEGM NAME=CUSTCHILD PARENT=CUSTROOT
FIELD NAME=CHILDID,START=1,LENGTH=5
FIELD NAME=UPDATED,START=6,LENGTH=1
PROCOPT=GO
END
`
	jclJob := `//PAYROLL JOB (ACCT),'PAYROLL',CLASS=A,MSGCLASS=X
//STEP1   EXEC PGM=PAYROLL
//INPUT   DD  DSN=CORP.PAYROLL.INPUT,DISP=SHR
//OUTPUT  DD  DSN=CORP.PAYROLL.OUTPUT,DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(5,5)),UNIT=SYSDA
//* VERIFY RESULTS BEFORE CATALOGING
`
	mqSource := `MQOPEN QUEUE1
MQPUT QUEUE1
MQGET QUEUE1
MQCLOSE QUEUE1
`
	asmSource := `SAMPLE   CSECT
SAMPLE   START 0
         USING *,15
         DC    CL8'HELLO'
* ensure message is displayed
`
	sqlSource := `SELECT ID FROM CUSTOMER_LOG;`

	files := map[string]string{
		"program.cbl":  cobolSource,
		"schema.ddl":   ddl,
		"database.dbd": imsDBD,
		"job.jcl":      jclJob,
		"module.asm":   asmSource,
		"queue.txt":    mqSource,
		"query.sql":    sqlSource,
	}
	for name, contents := range files {
		if err := os.WriteFile(filepath.Join(dir, name), []byte(contents), 0o644); err != nil {
			t.Fatalf("write file %s: %v", name, err)
		}
	}

	indexer := NewIndexer()
	docs, err := indexer.IndexRepo(context.Background(), dir, []string{"springboot"})
	if err != nil {
		t.Fatalf("IndexRepo returned error: %v", err)
	}

	hasFlow := false
	hasDb2 := false
	hasIMS := false
	hasCICS := false
	hasMQ := false
	hasModernization := false
	jclFlow := false
	jclWorking := false
	jclBusiness := false
	imsWorking := false
	imsBusiness := false
	db2Working := false
	db2Business := false
	mqWorking := false
	mqBusiness := false
	asmFlow := false
	asmWorking := false
	asmBusiness := false

	for _, doc := range docs {
		switch doc.Type {
		case "flow":
			hasFlow = true
			if hasTechnology(doc, "JCL") {
				jclFlow = true
			}
			if hasTechnology(doc, "IMS") {
				hasIMS = true
			}
			if hasTechnology(doc, "DB2") {
				hasDb2 = true
			}
			if hasTechnology(doc, "MQ") {
				hasMQ = true
			}
			if hasTechnology(doc, "Assembler") {
				asmFlow = true
			}
		case "working_notes":
			if hasTechnology(doc, "JCL") {
				jclWorking = true
			}
			if hasTechnology(doc, "IMS") {
				imsWorking = true
			}
			if hasTechnology(doc, "DB2") {
				db2Working = true
			}
			if hasTechnology(doc, "MQ") {
				mqWorking = true
			}
			if hasTechnology(doc, "Assembler") {
				asmWorking = true
			}
		case "business_rules":
			if hasTechnology(doc, "JCL") {
				jclBusiness = true
			}
			if hasTechnology(doc, "IMS") {
				imsBusiness = true
			}
			if hasTechnology(doc, "DB2") {
				db2Business = true
			}
			if hasTechnology(doc, "MQ") {
				mqBusiness = true
			}
			if hasTechnology(doc, "Assembler") {
				asmBusiness = true
			}
		case "db2_schema":
			if hasTechnology(doc, "DB2") {
				hasDb2 = true
			}
		case "ims_schema":
			if hasTechnology(doc, "IMS") {
				hasIMS = true
			}
		case "cics_flow":
			hasCICS = true
		case "mq_flow":
			hasMQ = true
		case "modernization":
			if strings.Contains(strings.ToLower(doc.Program), "springboot") {
				hasModernization = true
			}
		}
	}

	if !hasFlow {
		t.Fatalf("expected flow documentation")
	}
	if !hasDb2 {
		t.Fatalf("expected DB2 schema documentation with tags")
	}
	if !hasIMS {
		t.Fatalf("expected IMS schema documentation with tags")
	}
	if !hasCICS {
		t.Fatalf("expected CICS flow documentation")
	}
	if !hasMQ {
		t.Fatalf("expected MQ documentation")
	}
	if !hasModernization {
		t.Fatalf("expected modernization plan for springboot")
	}
	if !(jclFlow && jclWorking && jclBusiness) {
		t.Fatalf("expected JCL flow, working notes, and business rules")
	}
	if !(imsWorking && imsBusiness) {
		t.Fatalf("expected IMS working notes and business rules")
	}
	if !(db2Working && db2Business) {
		t.Fatalf("expected DB2 working notes and business rules")
	}
	if !(mqWorking && mqBusiness) {
		t.Fatalf("expected MQ working notes and business rules")
	}
	if !(asmFlow && asmWorking && asmBusiness) {
		t.Fatalf("expected assembler flow, working notes, and business rules")
	}
}

func hasTechnology(doc Doc, target string) bool {
	for _, tech := range doc.Technologies {
		if strings.EqualFold(strings.TrimSpace(tech), target) {
			return true
		}
	}
	return false
}
