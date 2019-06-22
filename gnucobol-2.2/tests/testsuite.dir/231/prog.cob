
       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       DATA  DIVISION.
       WORKING-STORAGE SECTION.
       78  const1    value 115.
       78  const2    value 200.
       77  othervar  PIC X(const1).
       01  var       PIC X(const2).

       PROCEDURE DIVISION.

           IF const1 = const2
           OR const2 = const1
              DISPLAY 'no way' END-DISPLAY
           END-IF

           EVALUATE const1
           WHEN  15
           WHEN 115
               display '1'
           WHEN  < 16
               move othervar (1:const1 - 8)
                 to var (17 - const1:const1 - 8)
           WHEN  > 16
               display othervar
       *> actually WHEN OTHER is also FALSE in this case (115 = 16),
       *> but this is too complex to check
           WHEN OTHER
               display othervar
           END-EVALUATE

           STOP RUN.
