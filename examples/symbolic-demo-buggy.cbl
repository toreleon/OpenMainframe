       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUGDEMO.
      *> --------------------------------------------------------
      *> Buggy version: fee can go negative due to sign error
      *> --------------------------------------------------------

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-BALANCE        PIC S9(7)V99 VALUE 1000.
       01  WS-AMOUNT         PIC S9(7)V99 VALUE 0.
       01  WS-FEE            PIC S9(5)V99 VALUE 0.
       01  WS-DIVISOR        PIC S9(5)    VALUE 0.
       01  WS-RESULT         PIC S9(7)V99 VALUE 0.

      *> This invariant will FAIL: the fee goes negative
      *> when AMOUNT <= 1000 because of the subtract bug.
      *> @INVARIANT: WS-FEE >= 0

      *> This safety check will FAIL: DIVISOR can be zero
      *> on the path where AMOUNT <= 0, causing divide-by-zero.
      *> @SAFETY: WS-DIVISOR = 0

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           IF WS-AMOUNT > 1000
               COMPUTE WS-FEE = WS-AMOUNT * 2 / 100
           ELSE
      *>       BUG: should be MOVE 5 TO WS-FEE, but we
      *>       subtract instead, making it negative.
               SUBTRACT 5 FROM WS-FEE
           END-IF.

      *>   BUG: DIVISOR is never set on the else path,
      *>   so it stays 0 from initialization.
           IF WS-AMOUNT > 0
               MOVE WS-AMOUNT TO WS-DIVISOR
           END-IF.

           IF WS-DIVISOR > 0
               DIVIDE WS-BALANCE BY WS-DIVISOR
                   GIVING WS-RESULT
           END-IF.

           STOP RUN.
