       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICSMENU.
      *
      * Menu program reached via XCTL from sign-on.
      * Displays menu, accepts selection, returns with TRANSID.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-OPTION              PIC X VALUE SPACES.
       01  WS-MESSAGE             PIC X(40) VALUE SPACES.
      *
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY 'MENU PROGRAM STARTING'.
           IF EIBCALEN > 0
               DISPLAY 'RECEIVED COMMAREA'
           END-IF.
           DISPLAY 'SENDING MENU MAP'.
           EXEC CICS SEND
                MAP('COMEN0A')
                MAPSET('COMEN00')
                ERASE
           END-EXEC.
           EXEC CICS RETURN
                TRANSID('MENU')
           END-EXEC.
           STOP RUN.
