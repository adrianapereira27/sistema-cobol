      ******************************************************************
      * Author: Adriana Pereira
      * Date: 23/06/2019
      * Purpose: Relatório de Clientes
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RELATORIO-CLIENTES.

       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.

           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT relat-clientes ASSIGN TO "relatClientes.txt"
                  ORGANIZATION  IS LINE SEQUENTIAL
                  ACCESS MODE   IS SEQUENTIAL
                  FILE STATUS   IS ws-resultado-acesso.

       DATA DIVISION.
       FILE SECTION.
           FD relat-clientes.
           01  rw-relat-clientes           PIC x(200).

       WORKING-STORAGE SECTION.
           01  ws-campos-work.
               03 ws-resultado-acesso      PIC x(02).
                  88 ws-acesso-invalido        VALUE "47", "48", "49".
                  88 ws-operacao-ok            VALUE "00", "02".
                  88 ws-eof-arquivo            VALUE "10".
                  88 ws-registro-inexistente   VALUE "23".
                  88 ws-registro-existente     VALUE "22".
                  88 ws-arquivo-inexistente    VALUE "35".

           01  WS-FILE-STATUS.
               03 WS-STATUS                PIC  X(02) VALUE SPACES.

           01  WS-CONTADORES.
               03 WS-CT-LIDOS              PIC  9(06) VALUE ZEROS.
               03 WS-CT-PAGINA             PIC  9(03) VALUE ZEROS.
               03 WS-CT-LINHAS             PIC  9(02) VALUE 99.

           01  WR-CAB1.
               03 FILLER                   PIC X(020) VALUE " "
               03 FILLER                   PIC X(040) VALUE
                   "RELATORIO DE CLIENTES".
           01  WR-CAB2.
               03 FILLER                   PIC X(009) VALUE
                   "FILTROS: "
               03 WR-DS-FILTROS-2          PIC X(060) VALUE SPACES.




       PROCEDURE DIVISION.
       MAIN-PROCEDURE.


           EXIT PROGRAM.
       END PROGRAM RELATORIO-CLIENTES.
