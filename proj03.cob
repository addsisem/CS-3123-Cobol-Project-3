       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJ03.
       AUTHOR. Addyson Sisemore
      * PROJECT  3.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'NEWEMP'.
           SELECT PRNT-FILE ASSIGN TO 'UR-S-PRNT'.

       DATA DIVISION.

       FILE SECTION.
       FD INPUT-FILE
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.
       01 INPUT-REC     PIC X(132).
       FD PRNT-FILE
           LABEL RECORDS ARE OMITTED.
       01 PRNT-REC      PIC X(125).
       WORKING-STORAGE SECTION.
       01 PAGE-CT       PIC 9999        VALUE '0001'.
       01 REC-CT        PIC 99          VALUE '00'.
       01 LN-CT         PIC 99          VALUE '00'.
       01 TOTAL-REC     PIC 9999        VALUE '0000'.
       01 TOTAL-DED     PIC 9(4)V99     VALUE '000000'.
       01 TOT-H-DED     PIC 9(6)V99     VALUE '0'.
       01 TOT-S-DED     PIC 9(6)V99     VALUE '0'.
       01 H-EMP         PIC 9999.
       01 S-EMP         PIC 9999.
       01 AVG-H         PIC 9(6)V99.
       01 AVG-S         PIC 9(6)V99.
      **************************************************************
      * LAYOUT FOR THE INPUT FILE *
      **************************************************************
       01 EMP-REC-TABLE.
         03     REC-ENTRIES OCCURS 100 TIMES.
                05      T-EID           PIC X(7).
                05      T-LAST          PIC X(15).
                05      T-FIRST         PIC X(15).
                05      T-TYPE          PIC X(2).
                05      T-TITLE         PIC X(17).
                05      T-SSN           PIC X(9).
                05      FILLER          PIC X(24)      VALUE '.'.
                05      T-DATE          PIC X(8).
                05      FILLER          PIC X(2)       VALUE SPACES.
                05      T-RATE          PIC 9(4)V99.
                05      T-STATUS        PIC X(1).
                05      T-DED1          PIC 9(3)V99.
                05      T-DED2          PIC 9(3)V99.
                05      T-DED3          PIC 9(3)V99.
                05      T-DED4          PIC 9(3)V99.
                05      T-DED5          PIC 9(3)V99.
         03     DEDUCTIONS OCCURS 5 TIMES.
                05      T-DED        PIC 9(3)V99.
       01 SUB           PIC 999         VALUE 001.
       01 SUB2          PIC 99          VALUE 01.
      **************************************************************
      * LAYOUT FOR THE DATA OF REPORT PRNTING *
      **************************************************************
       01 PRNT-DATA1.
         03 FILLER        PIC X(2)              VALUE SPACES.
         03 L-EID1        PIC X(7).
         03 FILLER        PIC X(2)              VALUE SPACES.
         03 L-SSN1        PIC XXXBXXBXXXX.
         03 FILLER        PIC X(2)              VALUE SPACES.
         03 L-LAST1       PIC X(15).
         03 FILLER        PIC X(1)              VALUE SPACES.
         03 L-FIRST1      PIC X(10).
         03 FILLER        PIC X(7)              VALUE SPACES.
         03 L-TYPE1       PIC X(2).
         03 FILLER        PIC X(6)              VALUE SPACES.
         03 L-TITLE1      PIC X(17).
         03 FILLER        PIC X(3)              VALUE SPACES.
          03 L-DATE1       PIC 99/99/9999.

       01 PRNT-DATA2.
         03 FILLER        PIC X(57)             VALUE SPACES.
         03 FILLER        PIC X(7)              VALUE 'DEDUCT:'.
         03 FILLER        PIC X(4)              VALUE SPACES.
         03 L-DED1        PIC ZZZ.99.
         03 FILLER        PIC X(2)              VALUE SPACES.
         03 FILLER        PIC X(5)              VALUE 'RATE:'.
         03 FILLER        PIC X(2)              VALUE SPACES.
         03 L-RATE1       PIC Z,ZZZ.99.
         03 FILLER        PIC X(2)              VALUE SPACES.
         03 FILLER        PIC X(6)              VALUE 'STATUS'.
         03 FILLER        PIC X(2)              VALUE SPACES.
         03 L-STATUS1     PIC X(1).

       01 PRNT-DATA3.
         03 FILLER        PIC X(68)             VALUE SPACES.
         03 L-DED2        PIC ZZZ.99.

       01 PRNT-DATA4.
         03 FILLER        PIC X(57)             VALUE SPACES.
         03 FILLER        PIC X(6)              VALUE 'TOTAL:'.
         03 FILLER        PIC X(2)              VALUE SPACES.
         03 L-TOTAL-DED1  PIC $Z,ZZ9.99.
      **************************************************************
      * LAYOUT FOR LAST PAGE OF REPORT PRINTING
      **************************************************************
       01 PRNT-LAST1.
         03 FILLER        PIC X(2)      VALUE SPACES.
         03 FILLER        VALUE 'NUMBER OF EMPLOYEE RECORDS READ:'.
         03 FILLER        PIC X(10)     VALUE SPACES.
         03 L-TOTAL-REC1  PIC ZZZ9.

       01 PRNT-LAST2.
         03 FILLER        PIC X(2)      VALUE SPACES.
         03 FILLER        VALUE 'NUMBER OF HOURLY EMPLOYEES:'.
         03 FILLER        PIC X(15)     VALUE SPACES.
             03 L-H-EMP1      PIC ZZZ9.
         03 FILLER        PIC X(2)      VALUE SPACES.
         03 FILLER        VALUE 'AVERAGE HOURLY RATE:'.
         03 FILLER        PIC X(7)      VALUE SPACES.
         03 L-AVG-H-EMP   PIC $ZZ9.99.
         03 FILLER        PIC X(10)     VALUE SPACES.
         03 FILLER        PIC X(13)     VALUE 'TOTAL DEDUCT:'.
         03 FILLER        PIC X(6)      VALUE SPACES.
         03 L-TOT-H-DED   PIC $ZZZ,ZZ9.99.

       01 PRNT-LAST3.
         03 FILLER        PIC X(2)      VALUE SPACES.
         03 FILLER        VALUE 'NUMBER OF SALARIED EMPLOYEES:'.
         03 FILLER        PIC X(13)     VALUE SPACES.
         03 L-S-EMP1      PIC ZZZ9.
         03 FILLER        PIC X(2)      VALUE SPACES.
         03 FILLER        VALUE 'AVERAGE SALARIED RATE:'.
         03 FILLER        PIC X(5)      VALUE SPACES.
         03 L-AVG-S-EMP   PIC $ZZ9.99.
         03 FILLER        PIC X(10)     VALUE SPACES.
         03 FILLER        PIC X(14)     VALUE 'TOTAL DEDUCT:'.
         03 FILLER        PIC X(5)      VALUE SPACES.
         03 L-TOT-S-DED   PIC $ZZZ,ZZ9.99.
      **************************************************************
      * LAYOUT FOR THE 1ST HEADING LINE OF REPORT PRINTING *
      **************************************************************
       01 PRNT-HEADING1.
         03 HL-CUR-YR   PIC 99.
         03 FILLER      PIC X(1)    VALUE '/'.
         03 HL-CUR-MO   PIC 99.
         03 FILLER      PIC X(1)    VALUE '/'.
         03 HL-CUR-DAY  PIC 99.
         03 FILLER      PIC X(51).
         03 FILLER      PIC X(28)   VALUE 'MASTERMIND COBOL, INC'.
         03 FILLER      PIC X(19)   VALUE SPACES.
         03 FILLER      PIC X(4)    VALUE 'PAGE'.
         03 HL-PAGE-CT  PIC ZZZ9.
      **************************************************************
      * LAYOUT FOR THE 2ND HEADING LINE OF REPORT PRINTING *
      **************************************************************
       01 PRNT-HEADING2.
         03 FILLER      PIC X(2)    VALUE SPACES.
         03 FILLER      PIC X(6)    VALUE 'EMP ID'.
         03 FILLER      PIC X(3)    VALUE SPACES.
         03 FILLER      PIC X(3)    VALUE 'SSN'.
         03 FILLER      PIC X(10)   VALUE SPACES.
         03 FILLER      PIC X(8)    VALUE 'LASTNAME'.
         03 FILLER      PIC X(8)    VALUE SPACES.
         03 FILLER      PIC X(9)    VALUE 'FIRSTNAME'.
         03 FILLER      PIC X(8)    VALUE SPACES.
         03 FILLER      PIC X(4)    VALUE 'TYPE'.
         03 FILLER      PIC X(4)    VALUE SPACES.
         03 FILLER      PIC X(5)    VALUE 'TITLE'.
         03 FILLER      PIC X(16)   VALUE SPACES.
         03 FILLER      PIC X(4)    VALUE 'DATE'.
      **************************************************************
      * LAYOUT FOR DATE
      **************************************************************
       01 CUR-DATE.
          05      CUR-YR      PIC 99.
          05      CUR-MO      PIC 99.
          05      CUR-DAY     PIC 99.
       01 MISC.
      **************************************************************
      *       END OF FILE (EOF) SWITCHES *
      *       0 = NOT AT EOF 1 = AT EOF *
      **************************************************************
         03 EOF-I      PIC 9   VALUE 0.
      **************************************************************
      *       START OF PROCEDURE DIVISION       *
      **************************************************************
       PROCEDURE DIVISION.
           ACCEPT CUR-DATE FROM DATE.
       000-MAINLINE.
           OPEN INPUT INPUT-FILE
                OUTPUT PRNT-FILE.
            PERFORM 2000-READ-INPUT.
           PERFORM 1400-PRINT-HEAD.
           MOVE 0 TO TOT-H-DED.
           MOVE 0 TO TOT-S-DED.
           PERFORM 1500-LOOP
             UNTIL EOF-I = 1.
           PERFORM 1700-LAST-PAGE.
           CLOSE INPUT-FILE
             PRNT-FILE.
           STOP RUN.
      ***************************************************************
      *       PRINT HEADERS
      ***************************************************************
       1400-PRINT-HEAD.
           MOVE CUR-YR TO HL-CUR-YR.
           MOVE CUR-MO TO HL-CUR-MO.
           MOVE CUR-DAY TO HL-CUR-DAY.
           MOVE PAGE-CT TO HL-PAGE-CT.
      /
           WRITE PRNT-REC FROM PRNT-HEADING1
             AFTER ADVANCING 1 LINE.
           ADD 1 TO PAGE-CT.
           MOVE SPACES TO PRNT-REC.
           WRITE PRNT-REC.

           WRITE PRNT-REC FROM PRNT-HEADING2
             AFTER ADVANCING 1 LINE.
           MOVE SPACES TO PRNT-REC.
           WRITE PRNT-REC
             AFTER ADVANCING 1 LINE.
           ADD 4 TO LN-CT.

       1450-PRINT-HEAD.
           MOVE PAGE-CT TO HL-PAGE-CT.
           WRITE PRNT-REC FROM PRNT-HEADING1
             AFTER ADVANCING 2 LINES.
           ADD 1 TO PAGE-CT.
           MOVE SPACES TO PRNT-REC.
               WRITE PRNT-REC.

           IF EOF-I IS NOT EQUAL TO 1 THEN
             ADD 4 TO LN-CT
             WRITE PRNT-REC FROM PRNT-HEADING2
               AFTER ADVANCING 1 LINE
             MOVE SPACES TO PRNT-REC
             WRITE PRNT-REC
             AFTER ADVANCING 1 LINE.
      **************************************************************
      * PROCEDURE LOOP FOR DATA
      **************************************************************
       1500-LOOP.
           PERFORM 2100-TRANSFER-DATA.
           ADD 1 TO REC-CT.
           MOVE 0 TO TOTAL-DED.
           PERFORM 2200-CALC-TOTALS UNTIL SUB2 > 5.
           MOVE 1 TO SUB2.
           PERFORM 1800-COUNT-STATUS.
           PERFORM 1600-PRINT-DATA.
           ADD 1 TO SUB.
           PERFORM 1625-PRINT-DATA UNTIL SUB2 > 4.
           MOVE 1 TO SUB2.
           PERFORM 1650-PRINT-DATA.
           ADD 7 TO LN-CT
           PERFORM 2000-READ-INPUT.

           IF REC-CT IS EQUAL TO 10 THEN
              ADD REC-CT TO TOTAL-REC
              SUBTRACT 10 FROM REC-CT.

           IF LN-CT IS GREATER THAN OR EQUAL TO 25 THEN
              MOVE 0 TO LN-CT
              PERFORM 1450-PRINT-HEAD.
      **************************************************************
      * PRINTS THE SCHEDULE INFORMATION *
      **************************************************************
       1600-PRINT-DATA.
           MOVE T-SSN(SUB)      TO L-SSN1.
           INSPECT L-SSN1 REPLACING ALL ' ' BY '-'.
           MOVE T-LAST(SUB)     TO L-LAST1.
           MOVE T-FIRST(SUB)    TO L-FIRST1.
           MOVE T-EID(SUB)      TO L-EID1.
           MOVE T-TITLE(SUB)    TO L-TITLE1.
           MOVE T-TYPE(SUB)     TO L-TYPE1.
           MOVE T-DATE(SUB)     TO L-DATE1.
           MOVE T-RATE(SUB)     TO L-RATE1.
           MOVE T-STATUS(SUB)   TO L-STATUS1.
           MOVE T-DED(SUB2) TO L-DED1.
             WRITE PRNT-REC FROM PRNT-DATA1
               AFTER ADVANCING 1 LINE.
             WRITE PRNT-REC FROM PRNT-DATA2
               AFTER ADVANCING 1 LINE.

       1625-PRINT-DATA.
           ADD 1 TO SUB2.
           MOVE T-DED(SUB2) TO L-DED2.
             WRITE PRNT-REC FROM PRNT-DATA3
               AFTER ADVANCING 1 LINE.

       1650-PRINT-DATA.
           MOVE TOTAL-DED TO L-TOTAL-DED1.
            WRITE PRNT-REC FROM PRNT-DATA4
               AFTER ADVANCING 1 LINE.
      **************************************************************
      * PRINTS TOTALS AND AVERAGES TO THE LAST PAGE
      **************************************************************
       1700-LAST-PAGE.
           PERFORM 1450-PRINT-HEAD.
           ADD REC-CT TO TOTAL-REC.
           MOVE TOTAL-REC TO L-TOTAL-REC1.
           MOVE H-EMP TO L-H-EMP1.
           MOVE S-EMP TO L-S-EMP1.
           MOVE TOT-H-DED TO L-TOT-H-DED.
           MOVE TOT-S-DED TO L-TOT-S-DED.
           PERFORM 1900-CALC-AVERAGES.
            WRITE PRNT-REC FROM PRNT-LAST1
               AFTER ADVANCING 1 LINE.
             WRITE PRNT-REC FROM PRNT-LAST2
               AFTER ADVANCING 1 LINE.
             WRITE PRNT-REC FROM PRNT-LAST3
               AFTER ADVANCING 1 LINE.
      **************************************************************
      * TRANSFER DATA TO DEDUCTIONS TABLE
      **************************************************************
       2100-TRANSFER-DATA.
           MOVE T-DED1(SUB) TO T-DED(1).
           MOVE T-DED2(SUB) TO T-DED(2).
           MOVE T-DED3(SUB) TO T-DED(3).
           MOVE T-DED4(SUB) TO T-DED(4).
           MOVE T-DED5(SUB) TO T-DED(5).
      **************************************************************
      * CALC TOTALS FOR DEDUCTIONS
      * ************************************************************
       2200-CALC-TOTALS.
           ADD T-DED(SUB2) TO TOTAL-DED.
           ADD 1 TO SUB2.
      **************************************************************
      * CALC TOTALS FOR HOURLY VS SALARIED EMPLOYEES
      **************************************************************
       1800-COUNT-STATUS.
           IF T-STATUS(SUB) IS EQUAL TO 'H' THEN
             ADD 1 TO H-EMP
             ADD T-RATE(SUB) TO AVG-H
             ADD TOTAL-DED TO TOT-H-DED.
           IF T-STATUS(SUB) IS EQUAL TO 'S' THEN
             ADD 1 TO S-EMP
             ADD T-RATE(SUB) TO AVG-S
             ADD TOTAL-DED TO TOT-S-DED.
      **************************************************************
      * CALCULATE AVERAGE RATES
      **************************************************************
       1900-CALC-AVERAGES.
           DIVIDE AVG-H BY H-EMP
                GIVING L-AVG-H-EMP.
           DIVIDE AVG-S BY S-EMP
             GIVING L-AVG-S-EMP.
      **************************************************************
      * READS THE INPUT FILE *
      **************************************************************
       2000-READ-INPUT.
           READ INPUT-FILE INTO REC-ENTRIES(SUB)
             AT END MOVE 1 TO EOF-I.       
