      *> COBCALC - Algebraic Expression Evaluator in COBOL
      *> Copyright (C) 2025 manny juan
      *>
      *> This program is free software: you can redistribute it and/or modify
      *> it under the terms of the GNU General Public License as published by
      *> the Free Software Foundation, either version 3 of the License, or
      *> (at your option) any later version.
      *>
      *> This program is distributed in the hope that it will be useful,
      *> but WITHOUT ANY WARRANTY; without even the implied warranty of
      *> MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
      *> GNU General Public License for more details.
      *>
      *> You should have received a copy of the GNU General Public License
      *> along with this program.  If not, see <https://www.gnu.org/licenses/>.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBCALC.
      *>> INCLUDES ^ EXPONENTIATION
      *>> -- SOLVE AMORT MONTHLY PMT: INT=5% LOAN=$250000 N=30 YRS
      *>>  (5/1200*250000*((1+5/1200)^(30*12)))/(((1+5/1200)^(30*12))-1)
      *>>  PMT=(1342.05)
       AUTHOR. MANNY JUAN.
       DATE-WRITTEN. 11/30/90.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
      *>> INPUT-OUTPUT SECTION..
       DATA DIVISION.
       WORKING-STORAGE SECTION.
         01 FILLER PIC 9 VALUE 0.
           88 NO-MORE-EXPR VALUE 1.
         01 EXPR-RECORD.
           05 FILLER PIC X(72).
         01 WRK-AREA.
           03 WRK-RESULT PIC Z(9)9.99999-.
         01 EXP-CONVERT-AREA.
           03 EXP-STRING PIC X(72).
           03 EXP-RS COMP-2.
           03 EXP-RC PIC S9(04) COMP.
         01 EXP-WORK-AREA.
           03 EXP-TOKEN-ITEM.
             05 EXP-TOKEN-TYPE PIC X(01).
               88 EXP-TOKEN-IS-ALF VALUE 'A'.
               88 EXP-TOKEN-IS-NUM VALUE '0'.
             05 EXP-TOKEN.
               07 EXP-TOKEN-CH PIC X(01) OCCURS 16 TIMES.
           03 EXP-TOKEN-STRING-AREA.
             05 FILLER PIC X(01).
               88 EXP-CHECK-UNARY VALUE 'Y'.
               88 EXP-DONT-CHECK-UNARY VALUE 'N'.
             05 EXP-TOKEN-STRING.
               07 EXP-TOKEN-STR-CH PIC X(01) OCCURS 120 TIMES.
           03 EXP-SUB-LIMITS-AREA.
             05 EXP-ITX-LIMIT PIC S9(03) COMP VALUE +120.
             05 EXP-MSX-LIMIT PIC S9(03) COMP VALUE +32.
             05 EXP-OPX-LIMIT PIC S9(03) COMP VALUE +32.
              05 EXP-RSX-LIMIT PIC S9(03) COMP VALUE +32.
           03 EXP-SUBSCRIPTS-AREA.
             05 EXP-LAST-ITX PIC S9(03) COMP.
             05 EXP-ITX PIC S9(03) COMP.
             05 EXP-OTX                     PIC S9(03) COMP.
             05 EXP-MSX                     PIC S9(03) COMP.
             05 EXP-OPX                     PIC S9(03) COMP.
             05 EXP-RSX                     PIC S9(03) COMP.
           03 EXP-MODE                      PIC X(08).
           03 EXP-MODE-STACK-AREA.
             05 EXP-MODE-STACK              PIC X(08) OCCURS 32 TIMES.
           03 EXP-OP                        PIC X(01).
           03 EXP-OP-STACK-AREA.
             05 EXP-OP-STACK                PIC X(01) OCCURS 32 TIMES.
           03 EXP-RS-STACK-AREA.
             05 EXP-RS-STACK                COMP-2 OCCURS 32 TIMES.
           03 EXP-WORK-REGISTERS.
             05 EXP-WORK-RS-A               COMP-2.
             05 EXP-WORK-RS-B               COMP-2.
         01 GN-WORK-AREA.
           03 GN-IX                            PIC S9(03) COMP.
           03 GN-SIGN                          PIC X(01).
           03 GN-WHOLE-NUMBER                  PIC S9(15) COMP-3.
           03 GN-DIVISOR                       PIC S9(13) COMP-3.
         01 GN-CONVERT-AREA.
           03 GN-INPUT.
             05 GN-INPUT-CHARS.
               07 GN-CH PIC X(01) OCCURS 33 TIMES.
             05 GN-INPUT-DIGITS REDEFINES GN-INPUT-CHARS.
               07 GN-DIGIT PIC 9(01) OCCURS 33 TIMES.
           03 GN-NUMBER-VALUE COMP-2.
           03 FILLER PIC X(01).
             88 GN-GOOD-NUMBER VALUE 'Y'.
             88 GN-BAD-NUMBER VALUE 'N'.
       PROCEDURE DIVISION.
       0100-SOLVE.
           DISPLAY 'ENTER EXPRESSION (OR END)'
           PERFORM 1070-GET-EXPR
           PERFORM
             1140-DO-SOLVER-TESTER
           UNTIL EXPR-RECORD = 'END'
           GOBACK.
       1070-GET-EXPR.
            ACCEPT EXPR-RECORD
            CONTINUE.
       1140-DO-SOLVER-TESTER.
           MOVE EXPR-RECORD TO EXP-STRING
           MOVE ZEROES TO EXP-RS
           PERFORM 1210-SOLVE
           DISPLAY 'EXP=' EXP-STRING
           MOVE EXP-RS TO WRK-RESULT
           IF EXP-RC = ZEROES
             DISPLAY 'ANS=' WRK-RESULT
           ELSE
             DISPLAY 'PARTIAL ANS=' WRK-RESULT
           END-IF
           PERFORM 1070-GET-EXPR
           CONTINUE.
       1210-SOLVE.
           MOVE EXP-STRING TO EXP-TOKEN-STRING
           MOVE SPACES TO EXP-MODE-STACK-AREA
           MOVE SPACES TO EXP-OP-STACK-AREA
           PERFORM VARYING EXP-RSX FROM 1 BY +1
           UNTIL EXP-RSX > EXP-RSX-LIMIT
             MOVE ZEROES TO EXP-RS-STACK (EXP-RSX)
           END-PERFORM
           MOVE +0 TO EXP-MSX
           MOVE +0 TO EXP-OPX
           MOVE +0 TO EXP-RSX
           MOVE +1 TO EXP-ITX
           MOVE SPACES TO EXP-MODE
           PERFORM 1490-PUSH-MODE
           MOVE ZEROES TO EXP-RC
           MOVE 'EXPR1' TO EXP-MODE
           PERFORM
             1280-PROCESS-EXP-MODE
           UNTIL EXP-MODE = SPACES
           OR NOT (EXP-RC = ZEROES)
           CONTINUE.
       1280-PROCESS-EXP-MODE.
           EVALUATE EXP-MODE
             WHEN ('EXPR1')
               MOVE 'EXPR2' TO EXP-MODE
               PERFORM 1490-PUSH-MODE
               MOVE 'TERM1' TO EXP-MODE
             WHEN ('EXPR2')
               PERFORM 1350-GET-TOKEN
               IF (EXP-TOKEN = '+' OR EXP-TOKEN = '-')
                 MOVE EXP-TOKEN TO EXP-OP
                 PERFORM 1560-PUSH-OP
                 MOVE 'EXPR3' TO EXP-MODE
                 PERFORM 1490-PUSH-MODE
                 MOVE 'TERM1' TO EXP-MODE
               ELSE
                 PERFORM 1420-UNGET-TOKEN
                 PERFORM 1700-POP-MODE
               END-IF
             WHEN ('EXPR3')
               PERFORM 1770-POP-OP
               EVALUATE (EXP-OP)
                 WHEN ('+')
                   PERFORM 1840-POP-RS
                   MOVE EXP-RS TO EXP-WORK-RS-A
                   PERFORM 1840-POP-RS
                   MOVE EXP-RS TO EXP-WORK-RS-B
                   COMPUTE EXP-RS = EXP-WORK-RS-B + EXP-WORK-RS-A
                   PERFORM 1630-PUSH-RS
                   MOVE 'EXPR2' TO EXP-MODE
                 WHEN ('-')
                   PERFORM 1840-POP-RS
                   MOVE EXP-RS TO EXP-WORK-RS-A
                   PERFORM 1840-POP-RS
                   MOVE EXP-RS TO EXP-WORK-RS-B
                   COMPUTE EXP-RS = EXP-WORK-RS-B - EXP-WORK-RS-A
                   PERFORM 1630-PUSH-RS
                   MOVE 'EXPR2' TO EXP-MODE
                 WHEN OTHER
                   PERFORM 1560-PUSH-OP
               END-EVALUATE
             WHEN ('TERM1')
               MOVE 'TERM2' TO EXP-MODE
               PERFORM 1490-PUSH-MODE
               MOVE 'FACT1' TO EXP-MODE
             WHEN ('TERM2')
               PERFORM 1350-GET-TOKEN
               EVALUATE EXP-TOKEN
                 WHEN ('^')
                   MOVE EXP-TOKEN TO EXP-OP
                   PERFORM 1560-PUSH-OP
                   MOVE 'TERM3' TO EXP-MODE
                   PERFORM 1490-PUSH-MODE
                   MOVE 'FACT1' TO EXP-MODE
                 WHEN ('*')
                   MOVE EXP-TOKEN TO EXP-OP
                   PERFORM 1560-PUSH-OP
                   MOVE 'TERM3' TO EXP-MODE
                   PERFORM 1490-PUSH-MODE
                   MOVE 'FACT1' TO EXP-MODE
                 WHEN ('/')
                   MOVE EXP-TOKEN TO EXP-OP
                   PERFORM 1560-PUSH-OP
                   MOVE 'TERM3' TO EXP-MODE
                   PERFORM 1490-PUSH-MODE
                   MOVE 'FACT1' TO EXP-MODE
                 WHEN OTHER
                   PERFORM 1420-UNGET-TOKEN
                   PERFORM 1700-POP-MODE
               END-EVALUATE
             WHEN ('TERM3')
               PERFORM 1770-POP-OP
               EVALUATE (EXP-OP)
                 WHEN ('^')
                   PERFORM 1840-POP-RS
                   MOVE EXP-RS TO EXP-WORK-RS-A
                   PERFORM 1840-POP-RS
                   MOVE EXP-RS TO EXP-WORK-RS-B
                   COMPUTE EXP-RS = EXP-WORK-RS-B ** EXP-WORK-RS-A
                   PERFORM 1630-PUSH-RS
                   MOVE 'TERM2' TO EXP-MODE
                 WHEN ('*')
                   PERFORM 1840-POP-RS
                   MOVE EXP-RS TO EXP-WORK-RS-A
                   PERFORM 1840-POP-RS
                   MOVE EXP-RS TO EXP-WORK-RS-B
                   COMPUTE EXP-RS = EXP-WORK-RS-B * EXP-WORK-RS-A
                   PERFORM 1630-PUSH-RS
                   MOVE 'TERM2' TO EXP-MODE
                 WHEN ('/')
                   PERFORM 1840-POP-RS
                   MOVE EXP-RS TO EXP-WORK-RS-A
                   PERFORM 1840-POP-RS
                   MOVE EXP-RS TO EXP-WORK-RS-B
                   COMPUTE EXP-RS = EXP-WORK-RS-B / EXP-WORK-RS-A
                   PERFORM 1630-PUSH-RS
                   MOVE 'TERM2' TO EXP-MODE
                 WHEN OTHER
                   PERFORM 1560-PUSH-OP
               END-EVALUATE
             WHEN ('FACT1')
               SET EXP-CHECK-UNARY TO TRUE
               PERFORM 1350-GET-TOKEN
               EVALUATE TRUE
                 WHEN (EXP-TOKEN = 'SQRT')
                   PERFORM 1350-GET-TOKEN
                   IF (EXP-TOKEN NOT = '(')
                     DISPLAY '( EXPECTED AFTER FUNC, FOUND ' EXP-TOKEN
                     MOVE +1 TO EXP-RC
                   ELSE
                     MOVE 'FACT3' TO EXP-MODE
                     PERFORM 1490-PUSH-MODE
                     MOVE 'EXPR1' TO EXP-MODE
                   END-IF
                 WHEN (EXP-TOKEN-IS-NUM)
                   MOVE EXP-TOKEN TO GN-INPUT
                   PERFORM 1910-GET-NUMBER
                   MOVE GN-NUMBER-VALUE TO EXP-RS
                   PERFORM 1630-PUSH-RS
                   PERFORM 1700-POP-MODE
                 WHEN (EXP-TOKEN = '(')
                   MOVE 'FACT2' TO EXP-MODE
                   PERFORM 1490-PUSH-MODE
                   MOVE 'EXPR1' TO EXP-MODE
                 WHEN OTHER
                   DISPLAY 'NUMBER OR SQRT EXPECTED, FOUND ' EXP-TOKEN
                   MOVE +1 TO EXP-RC
               END-EVALUATE
             WHEN ('FACT2')
               PERFORM 1350-GET-TOKEN
               IF (NOT (EXP-TOKEN = ')'))
                 DISPLAY ') EXPECTED, FOUND ' EXP-TOKEN
                 MOVE +1 TO EXP-RC
               END-IF
               PERFORM 1700-POP-MODE
             WHEN ('FACT3')
               PERFORM 1350-GET-TOKEN
               IF (NOT (EXP-TOKEN = ')'))
                 DISPLAY ') EXPECTED, FOUND ' EXP-TOKEN
                 MOVE +1 TO EXP-RC
               ELSE
                 PERFORM 1840-POP-RS
                 IF (EXP-RS < 0) THEN
                   DISPLAY 'INVALID ARGUMENT TO SQRT'
                   MOVE +1 TO EXP-RC
                 ELSE
                   COMPUTE EXP-RS = EXP-RS ** 0.5
                 END-IF
                 PERFORM 1630-PUSH-RS
               END-IF
               PERFORM 1700-POP-MODE
           END-EVALUATE
           CONTINUE.

       1350-GET-TOKEN.
           MOVE EXP-ITX TO EXP-LAST-ITX
           MOVE SPACES TO EXP-TOKEN-TYPE
           MOVE SPACES TO EXP-TOKEN
           MOVE +0 TO EXP-OTX
      *>>     --SKIP LEADING SPACES
           PERFORM
             VARYING EXP-ITX FROM EXP-ITX BY +1
           UNTIL EXP-TOKEN-STR-CH (EXP-ITX) NOT = SPACE
           OR NOT (EXP-ITX < EXP-ITX-LIMIT)
             CONTINUE
           END-PERFORM
           EVALUATE TRUE
             WHEN (EXP-TOKEN-STR-CH (EXP-ITX) ALPHABETIC)
               PERFORM TEST BEFORE
               UNTIL EXP-TOKEN-STR-CH (EXP-ITX) = SPACE
               OR EXP-TOKEN-STR-CH (EXP-ITX) NOT ALPHABETIC
                 COMPUTE EXP-OTX = EXP-OTX + 1
                 MOVE EXP-TOKEN-STR-CH (EXP-ITX)
                   TO EXP-TOKEN-CH (EXP-OTX)
                 COMPUTE EXP-ITX = EXP-ITX + 1
               END-PERFORM
               SET EXP-TOKEN-IS-ALF TO TRUE
             WHEN (EXP-TOKEN-STR-CH (EXP-ITX) = '('
               OR EXP-TOKEN-STR-CH (EXP-ITX) = ')')
               COMPUTE EXP-OTX = EXP-OTX + 1
               MOVE EXP-TOKEN-STR-CH (EXP-ITX)
                 TO EXP-TOKEN-CH (EXP-OTX)
               COMPUTE EXP-ITX = EXP-ITX + 1
             WHEN (NOT (EXP-CHECK-UNARY)
               AND (EXP-TOKEN-STR-CH (EXP-ITX) = '+'
               OR EXP-TOKEN-STR-CH (EXP-ITX) = '-'
               OR EXP-TOKEN-STR-CH (EXP-ITX) = '^'
              OR EXP-TOKEN-STR-CH (EXP-ITX) = '*'
               OR EXP-TOKEN-STR-CH (EXP-ITX) = '/'))
               COMPUTE EXP-OTX = EXP-OTX + 1
               MOVE EXP-TOKEN-STR-CH (EXP-ITX)
                 TO EXP-TOKEN-CH (EXP-OTX)
               COMPUTE EXP-ITX = EXP-ITX + 1
             WHEN EXP-CHECK-UNARY
               IF (EXP-TOKEN-STR-CH (EXP-ITX) = '-')
                 COMPUTE EXP-OTX = EXP-OTX + 1
                 MOVE EXP-TOKEN-STR-CH (EXP-ITX)
                   TO EXP-TOKEN-CH (EXP-OTX)
                 COMPUTE EXP-ITX = EXP-ITX + 1
               END-IF
               IF (EXP-TOKEN-STR-CH (EXP-ITX) NUMERIC)
                 SET EXP-TOKEN-IS-NUM TO TRUE
                 PERFORM TEST BEFORE
                 UNTIL EXP-TOKEN-STR-CH (EXP-ITX) NOT NUMERIC
                 OR EXP-TOKEN-STR-CH (EXP-ITX) = SPACE
                 OR EXP-TOKEN-STR-CH (EXP-ITX) = '.'
                   COMPUTE EXP-OTX = EXP-OTX + 1
                   MOVE EXP-TOKEN-STR-CH (EXP-ITX)
                     TO EXP-TOKEN-CH (EXP-OTX)
                   COMPUTE EXP-ITX = EXP-ITX + 1
                   PERFORM
                   VARYING EXP-ITX FROM EXP-ITX BY +1
                   UNTIL NOT (EXP-TOKEN-STR-CH (EXP-ITX) = ',')
                     CONTINUE
                   END-PERFORM
                 END-PERFORM
                 IF EXP-TOKEN-STR-CH (EXP-ITX) = '.'
                   COMPUTE EXP-OTX = EXP-OTX + 1
                   MOVE EXP-TOKEN-STR-CH (EXP-ITX)
                     TO EXP-TOKEN-CH (EXP-OTX)
                   COMPUTE EXP-ITX = EXP-ITX + 1
                   PERFORM TEST BEFORE
                   UNTIL EXP-TOKEN-STR-CH (EXP-ITX) NOT NUMERIC
                   OR EXP-TOKEN-STR-CH (EXP-ITX) = SPACE
                     COMPUTE EXP-OTX = EXP-OTX + 1
                     MOVE EXP-TOKEN-STR-CH (EXP-ITX)
                       TO EXP-TOKEN-CH (EXP-OTX)
                     COMPUTE EXP-ITX = EXP-ITX + 1
                   END-PERFORM
                 END-IF
               ELSE
                 COMPUTE EXP-OTX = EXP-OTX + 1
                 MOVE EXP-TOKEN-STR-CH (EXP-ITX)
                   TO EXP-TOKEN-CH (EXP-OTX)
                 COMPUTE EXP-ITX = EXP-ITX + 1
               END-IF
             WHEN OTHER
               DISPLAY 'UNKNOWN SYMBOL ' EXP-TOKEN
               MOVE +1 TO EXP-RC
           END-EVALUATE
      *>>     --RESET UNARY MINUS CHECK
           SET EXP-DONT-CHECK-UNARY TO TRUE
           CONTINUE.
       1420-UNGET-TOKEN.
           MOVE EXP-LAST-ITX TO EXP-ITX
           CONTINUE.
       1490-PUSH-MODE.
           COMPUTE EXP-MSX = EXP-MSX + 1
           MOVE EXP-MODE TO EXP-MODE-STACK (EXP-MSX)
           CONTINUE.
       1560-PUSH-OP.
           COMPUTE EXP-OPX = EXP-OPX + 1
           MOVE EXP-OP TO EXP-OP-STACK (EXP-OPX)
           CONTINUE.
       1630-PUSH-RS.
           COMPUTE EXP-RSX = EXP-RSX + 1
           MOVE EXP-RS TO EXP-RS-STACK (EXP-RSX)
           CONTINUE.
       1700-POP-MODE.
           MOVE EXP-MODE-STACK (EXP-MSX) TO EXP-MODE
           COMPUTE EXP-MSX = EXP-MSX - 1
           CONTINUE.
       1770-POP-OP.
           MOVE EXP-OP-STACK (EXP-OPX) TO EXP-OP
           COMPUTE EXP-OPX = EXP-OPX - 1
           CONTINUE.
       1840-POP-RS.
           MOVE EXP-RS-STACK (EXP-RSX) TO EXP-RS
           COMPUTE EXP-RSX = EXP-RSX - 1
           CONTINUE.
       1910-GET-NUMBER.
           MOVE 1 TO GN-IX
           MOVE SPACES TO GN-SIGN
           IF NOT (GN-INPUT = SPACES)
      *>>     --SKIP LEADING SPACES
             PERFORM VARYING GN-IX FROM GN-IX BY +1
             UNTIL GN-CH (GN-IX) NOT = SPACE
               CONTINUE
             END-PERFORM
             MOVE ZEROES TO GN-WHOLE-NUMBER
             MOVE 1 TO GN-DIVISOR
             IF (GN-CH (GN-IX) = '-')
               MOVE '-'TO GN-SIGN
               COMPUTE GN-IX = GN-IX + 1
             END-IF
             PERFORM TEST BEFORE
             UNTIL GN-CH (GN-IX) NOT NUMERIC
             OR GN-CH (GN-IX) = SPACE
             OR GN-CH (GN-IX) = '.'
               COMPUTE GN-WHOLE-NUMBER = 10 * GN-WHOLE-NUMBER
               + GN-DIGIT (GN-IX)
               COMPUTE GN-IX = GN-IX + 1
               PERFORM
               VARYING GN-IX FROM GN-IX BY +1
               UNTIL NOT (GN-CH (GN-IX) = ',')
                 CONTINUE
               END-PERFORM
             END-PERFORM
             IF GN-CH (GN-IX) = '.'
               COMPUTE GN-IX = GN-IX + 1
               PERFORM
                 TEST BEFORE
               UNTIL GN-CH (GN-IX) NOT NUMERIC
               OR GN-CH (GN-IX) = SPACE
                 COMPUTE GN-DIVISOR = 10 * GN-DIVISOR
                 COMPUTE GN-WHOLE-NUMBER = 10 * GN-WHOLE-NUMBER
                 + GN-DIGIT (GN-IX)
                 COMPUTE GN-IX = GN-IX + 1
               END-PERFORM
             END-IF
             COMPUTE GN-NUMBER-VALUE = GN-WHOLE-NUMBER / GN-DIVISOR
             IF GN-SIGN = '-'
               COMPUTE GN-NUMBER-VALUE = 0 - GN-NUMBER-VALUE
             END-IF
             IF GN-CH (GN-IX) = SPACE
               SET GN-GOOD-NUMBER TO TRUE
             ELSE
               SET GN-BAD-NUMBER TO TRUE
             END-IF
           END-IF
           CONTINUE.
       END PROGRAM COBCALC.
