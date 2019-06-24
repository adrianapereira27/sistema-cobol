      ******************************************************************
      * Author: Adriana Pereira
      * Date: 23/06/2019
      * Purpose: Validar CNPJ
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALIDAR-CNPJ.
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01  WS-TRABALHO.
           05 SALVA-CNPJ               PIC  9(014) VALUE ZERO.
           05 TESTE-77                 PIC  9(001) VALUE ZERO.
           05 LIXO                     PIC  9(006) VALUE ZERO.
           05 DV                       PIC  9(006) VALUE ZERO.
           05 RESTO                    PIC  9(002) VALUE ZERO.
           05 FILLER REDEFINES RESTO.
              10 R-1                   PIC  9(001).
              10 R-2                   PIC  9(001).

       LINKAGE SECTION.
       01  PARAMETROS-CNPJ.
           05 CNPJ                         PIC  9(014).
              88 EXCESSAO VALUE 62580000000084
                                78408606000151
                                92875673000163.
           05 FILLER REDEFINES CNPJ.
              10 CNPJ-01                   PIC  9(001).
              10 CNPJ-02                   PIC  9(001).
              10 CNPJ-03                   PIC  9(001).
              10 CNPJ-04                   PIC  9(001).
              10 CNPJ-05                   PIC  9(001).
              10 CNPJ-06                   PIC  9(001).
              10 CNPJ-07                   PIC  9(001).
              10 CNPJ-08                   PIC  9(001).
              10 CNPJ-09                   PIC  9(001).
              10 CNPJ-10                   PIC  9(001).
              10 CNPJ-11                   PIC  9(001).
              10 CNPJ-12                   PIC  9(001).
              10 CNPJ-13                   PIC  9(002).
              10 FILLER REDEFINES CNPJ-13.
                 15 CNPJ-14                PIC  9(001).
                 15 CNPJ-15                PIC  9(001).

           05 CNPJ-RETORNO                 PIC  X(002).
           05 FILLER  REDEFINES CNPJ-RETORNO.
              10 RETORNO-1                 PIC  X(001).
              10 RETORNO-2                 PIC  X(001).

       PROCEDURE DIVISION USING PARAMETROS-CNPJ.
       MAIN-PROCEDURE.
           IF   CNPJ-RETORNO EQUAL "77"
                MOVE CNPJ              TO SALVA-CNPJ
                MOVE 1                 TO TESTE-77
                EXIT PROGRAM
           ELSE
                IF  TESTE-77 EQUAL 1
                    MOVE SALVA-CNPJ    TO CNPJ
                    MOVE 0             TO TESTE-77
                    MOVE "78"          TO CNPJ-RETORNO
                    EXIT PROGRAM
                END-IF
           END-IF.

           MOVE "11"                   TO CNPJ-RETORNO

           COMPUTE DV  = CNPJ-01 *  5
                       + CNPJ-02 *  4
                       + CNPJ-03 *  3
                       + CNPJ-04 *  2
                       + CNPJ-05 *  9
                       + CNPJ-06 *  8
                       + CNPJ-07 *  7
                       + CNPJ-08 *  6
                       + CNPJ-09 *  5
                       + CNPJ-10 *  4
                       + CNPJ-11 *  3
                       + CNPJ-12 *  2

           DIVIDE 11 INTO DV GIVING LIXO REMAINDER RESTO

           IF   RESTO EQUAL 0 OR 1
                MOVE 0                 TO RESTO
           ELSE
                COMPUTE RESTO = RESTO - 11
           END-IF.

           IF   CNPJ-14 EQUAL RESTO
                MOVE "0" TO RETORNO-1
                COMPUTE DV  = CNPJ-01 *  6
                            + CNPJ-02 *  5
                            + CNPJ-03 *  4
                            + CNPJ-04 *  3
                            + CNPJ-05 *  2
                            + CNPJ-06 *  9
                            + CNPJ-07 *  8
                            + CNPJ-08 *  7
                            + CNPJ-09 *  6
                            + CNPJ-10 *  5
                            + CNPJ-11 *  4
                            + CNPJ-12 *  3
                            + CNPJ-14 *  2
                DIVIDE 11 INTO DV GIVING LIXO REMAINDER RESTO
                IF   RESTO EQUAL 0 OR 1
                     MOVE 0            TO RESTO
                     IF   CNPJ-15 EQUAL RESTO
                          MOVE "0"     TO RETORNO-2
                     END-IF
                ELSE
                     COMPUTE RESTO = RESTO - 11
                     IF   CNPJ-15 EQUAL RESTO
                          MOVE "0"     TO RETORNO-2
                     END-IF
                END-IF
           END-IF.

           IF   CNPJ EQUAL ZERO
           OR   CNPJ EQUAL 99999999999
                MOVE "99"              TO CNPJ-RETORNO
           END-IF.

           IF   EXCESSAO
                MOVE "00"              TO CNPJ-RETORNO
           END-IF.

           EXIT PROGRAM.
       END PROGRAM VALIDAR-CNPJ.
