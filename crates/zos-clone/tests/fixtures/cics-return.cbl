       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICSTEST.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID             PIC X(4) VALUE 'MENU'.
       01  WS-RESP               PIC 9(8) VALUE 0.
       01  WS-MESSAGE            PIC X(40) VALUE SPACES.
      *
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           MOVE 'CICS TEST STARTING' TO WS-MESSAGE.
           DISPLAY WS-MESSAGE.
           EXEC CICS RETURN
                TRANSID(WS-TRANID)
           END-EXEC.
           DISPLAY 'SHOULD NOT REACH HERE'.
           STOP RUN.
