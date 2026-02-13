       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICSLOGIN.
      *
      * Multi-screen test: login → XCTL to menu → RETURN.
      * Tests XCTL chain + COMMAREA passing.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-COMMAREA             PIC X(20) VALUE SPACES.
      *
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY 'LOGIN SCREEN'.
           MOVE 'AUTHENTICATED' TO WS-COMMAREA.
           DISPLAY 'XCTL TO MENU PROGRAM'.
           EXEC CICS XCTL
                PROGRAM('CICSMENU')
                COMMAREA(WS-COMMAREA)
                LENGTH(20)
           END-EXEC.
           DISPLAY 'SHOULD NOT REACH HERE'.
           STOP RUN.
