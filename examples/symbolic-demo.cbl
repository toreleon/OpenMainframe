       IDENTIFICATION DIVISION.
       PROGRAM-ID. SYMBDEMO.
      *> --------------------------------------------------------
      *> Symbolic Execution Demo
      *> A small balance-transfer program with verification
      *> annotations that the engine can check.
      *> --------------------------------------------------------

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-BALANCE        PIC S9(7)V99 VALUE 1000.
       01  WS-AMOUNT         PIC S9(7)V99 VALUE 0.
       01  WS-NEW-BALANCE    PIC S9(7)V99 VALUE 0.
       01  WS-FEE            PIC S9(5)V99 VALUE 0.
       01  WS-STATUS         PIC X(2)     VALUE SPACES.
       01  WS-ERROR-FLAG     PIC 9        VALUE 0.

      *> @INVARIANT: WS-FEE >= 0
      *> @SAFETY: WS-ERROR-FLAG = 2

       PROCEDURE DIVISION.
       MAIN-LOGIC.
      *> @PRE: WS-AMOUNT > 0
      *> @POST: WS-NEW-BALANCE >= 0

           IF WS-AMOUNT > 0
               PERFORM COMPUTE-FEE
               SUBTRACT WS-AMOUNT FROM WS-BALANCE
                   GIVING WS-NEW-BALANCE
               SUBTRACT WS-FEE FROM WS-NEW-BALANCE
               IF WS-NEW-BALANCE < 0
                   MOVE 1 TO WS-ERROR-FLAG
                   MOVE 'OD' TO WS-STATUS
               ELSE
                   MOVE 0 TO WS-ERROR-FLAG
                   MOVE 'OK' TO WS-STATUS
               END-IF
           ELSE
               MOVE 99 TO WS-ERROR-FLAG
               MOVE 'NA' TO WS-STATUS
           END-IF.

           STOP RUN.

       COMPUTE-FEE.
           IF WS-AMOUNT > 5000
               COMPUTE WS-FEE = WS-AMOUNT * 2 / 100
           ELSE IF WS-AMOUNT > 1000
               COMPUTE WS-FEE = WS-AMOUNT * 1 / 100
           ELSE
               MOVE 5 TO WS-FEE
           END-IF.
