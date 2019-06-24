      ******************************************************************
      * Author: Adriana Pereira
      * Date: 23/06/2019
      * Purpose: Validar CPF
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALIDAR-CPF.
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01  WS-TRABALHO.
           05 LIXO                     PIC  9(006) VALUE ZERO.
           05 DV                       PIC  9(006) VALUE ZERO.
           05 RESTO                    PIC  9(002) VALUE ZERO.

       LINKAGE SECTION.
       01  PARAMETROS-CPF.
           05 CPF                         PIC  9(011).
              88 CPF-INVALIDO             VALUE 11111111111
                                                22222222222
                                                33333333333
                                                44444444444
                                                55555555555
                                                66666666666
                                                77777777777
                                                88888888888
                                                99999999999
                                                00000000000.
           05 FILLER REDEFINES CPF.
              10 CPF-01                   PIC  9(001).
              10 CPF-02                   PIC  9(001).
              10 CPF-03                   PIC  9(001).
              10 CPF-04                   PIC  9(001).
              10 CPF-05                   PIC  9(001).
              10 CPF-06                   PIC  9(001).
              10 CPF-07                   PIC  9(001).
              10 CPF-08                   PIC  9(001).
              10 CPF-09                   PIC  9(001).
              10 CPF-10                   PIC  9(002).
              10 FILLER REDEFINES CPF-10.
                 15 CPF-11                PIC  9(001).
                 15 CPF-12                PIC  9(001).
           05 CPF-RETORNO.
              10 RETORNO-1                PIC  X(001).
              10 RETORNO-2                PIC  X(001).

       PROCEDURE DIVISION USING PARAMETROS-CPF.
       MAIN-PROCEDURE.
           IF   CPF-INVALIDO
                MOVE "99"              TO CPF-RETORNO
                EXIT PROGRAM
           END-IF.

           MOVE "11"                   TO CPF-RETORNO

           COMPUTE DV  = CPF-01 * 10
                       + CPF-02 *  9
                       + CPF-03 *  8
                       + CPF-04 *  7
                       + CPF-05 *  6
                       + CPF-06 *  5
                       + CPF-07 *  4
                       + CPF-08 *  3
                       + CPF-09 *  2

           DIVIDE 11 INTO DV GIVING LIXO REMAINDER RESTO

           IF   RESTO EQUAL 0 OR 1
                MOVE 0                 TO RESTO
           ELSE
                COMPUTE RESTO = RESTO - 11
           END-IF.

           IF   CPF-11 EQUAL RESTO
                MOVE "0"               TO RETORNO-1
                COMPUTE DV  = CPF-01 * 11
                            + CPF-02 * 10
                            + CPF-03 *  9
                            + CPF-04 *  8
                            + CPF-05 *  7
                            + CPF-06 *  6
                            + CPF-07 *  5
                            + CPF-08 *  4
                            + CPF-09 *  3
                            + CPF-11 *  2
                DIVIDE 11 INTO DV GIVING LIXO REMAINDER RESTO
                IF   RESTO EQUAL 0 OR 1
                     MOVE 0            TO RESTO
                     IF   CPF-12 EQUAL RESTO
                          MOVE "0"     TO RETORNO-2
                     END-IF
                ELSE
                     COMPUTE RESTO = RESTO - 11
                     IF   CPF-12 EQUAL RESTO
                          MOVE "0"     TO RETORNO-2
                     END-IF
                END-IF
           END-IF.

           EXIT PROGRAM.
       END PROGRAM VALIDAR-CPF.
