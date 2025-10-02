       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANSACTION-LOADER.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z15.
       OBJECT-COMPUTER. IBM-Z15.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANS-FILE ASSIGN TO 'transactions.dat'.
           SELECT REPORT-FILE ASSIGN TO 'report.txt'.
       DATA DIVISION.
       FILE SECTION.
       FD  TRANS-FILE.
       01  TRANS-REC.
           05 TRANS-ID         PIC X(12).
           05 TRANS-TYPE       PIC X(4).
           05 TRANS-AMOUNT     PIC S9(9)V99.
       FD  REPORT-FILE.
       01  REPORT-REC.
           05 REPORT-LINE      PIC X(80).
       WORKING-STORAGE SECTION.
       01  WS-TOTAL-DEBITS     PIC S9(11)V99 VALUE 0.
       01  WS-TOTAL-CREDITS    PIC S9(11)V99 VALUE 0.
       01  WS-LINE             PIC X(80).
       01  WS-EOF              PIC X VALUE 'N'.
       PROCEDURE DIVISION.
       MAIN-LOOP.
           OPEN INPUT TRANS-FILE
                OUTPUT REPORT-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ TRANS-FILE
                   AT END MOVE 'Y' TO WS-EOF
               END-READ
               IF WS-EOF NOT = 'Y'
                   EVALUATE TRANS-TYPE
                       WHEN 'CR'
                           ADD TRANS-AMOUNT TO WS-TOTAL-CREDITS
                       WHEN 'DB'
                           ADD TRANS-AMOUNT TO WS-TOTAL-DEBITS
                       WHEN OTHER
                           DISPLAY 'Unknown transaction type'
                   END-EVALUATE
               END-IF
           END-PERFORM
           STRING 'TOTAL CREDITS: ' DELIMITED BY SIZE
                  WS-TOTAL-CREDITS DELIMITED BY SIZE
                  INTO WS-LINE
           WRITE REPORT-REC FROM WS-LINE
           CLOSE TRANS-FILE REPORT-FILE
           STOP RUN.
