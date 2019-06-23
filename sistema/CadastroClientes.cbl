      ******************************************************************
      * Author: Adriana Pereira
      * Date: 22/06/2019
      * Purpose: Cadastro de Clientes
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADASTRO-CLIENTES.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT arq-clientes ASSIGN TO DISK "clientes.dat"
             ORGANIZATION       IS INDEXED
             ACCESS MODE        IS DYNAMIC
             RECORD KEY         IS rw-chave-1
             LOCK MODE          IS MANUAL
             FILE STATUS        IS ws-resultado-acesso.

       DATA DIVISION.
       FILE SECTION.
           FD arq-clientes.
           01 rw-chave-1.
               03 rw-nr-cnpj               PIC 9(014).
               03 rw-cd-cliente            PIC 9(007).
           01 rw-ds-razao-social           PIC x(040).
           01 rw-nr-latitude               PIC s9(003)v9(008).
           01 rw-nr-longitude              PIC s9(003)v9(008).

       WORKING-STORAGE SECTION.
           77  whs-mensagem                PIC x(200).
               88 processamento-sem-erro       VALUE SPACES.

           01  ws-campos-work.
               03 ws-resultado-acesso      PIC x(02).
                  88 ws-acesso-invalido        VALUE "47", "48", "49".
                  88 ws-operacao-ok            VALUE "00", "02".
                  88 ws-eof-arquivo            VALUE "10".
                  88 ws-registro-inexistente   VALUE "23".
                  88 ws-registro-existente     VALUE "22".
                  88 ws-arquivo-inexistente    VALUE "35".
               03 ws-ds-diretorio          PIC x(60) VALUE SPACES.
               03 ws-nr-cnpj-scr           PIC 9(14) VALUE ZEROS.
               03 ws-ds-rz-social-scr      PIC x(40) VALUE SPACES.
               03 ws-nr-latitude-scr       PIC s9(03)v9(08) VALUE ZEROS.
               03 ws-nr-longitude-scr      PIC s9(03)v9(08) VALUE ZEROS.
               03 ws-id-opcao              PIC x(01) VALUE SPACES.

           01 ws-campos-importacao.
               03 ws-cd-cliente            PIC 9(007).
               03 ws-nr-cnpj               PIC 9(014).
               03 ws-ds-razao-social       PIC x(040).
               03 ws-nr-latitude           PIC s9(003)v9(008).
               03 ws-nr-longitude          PIC s9(003)v9(008).

       SCREEN SECTION.
       01  DATA-ENTRY-SCREEN.
           05  MENU-SECTION.
               07  VALUE "MENU: "              LINE 05 COL 05.
               07  VALUE "1 - INCLUIR"         LINE 07 COL 10.
               07  VALUE "2 - ALTERAR"         LINE 08 COL 10.
               07  VALUE "3 - EXCLUIR"         LINE 09 COL 10.
               07  VALUE "4 - IMPORTAR"        LINE 10 COL 10.
               07  VALUE "9 - VOLTAR"          LINE 11 COL 10.
               07  VALUE "ESCOLHA A OPCAO: "   LINE 13 COL 05.
               07  OPCAO-ON-SCR-IN             LINE 13 COL 30
                       PIC x(01)               TO ws-id-opcao.

           05  DADOS-SECTION.
               07  VALUE "SISTEMA AMBEV"       BLANK SCREEN
                                               LINE 02 COL 30.
               07  VALUE "CADASTRO DE CLIENTES" LINE 03 COL 26.
           05  CNPJ-SECTION.
               07  VALUE "Informe os dados do cliente: "
                                               LINE 05 COL 05.
               07  VALUE "CNPJ:"               BLANK SCREEN
                                               LINE 06 COL 05.
               07  CNPJ-ON-SCR-IN              LINE 06 COL 15
                            PIC 9(14)        TO ws-nr-cnpj-scr.
           05  RAZAO-SECTION.
               07  VALUE "Razao social:"       LINE 07 COL 10.
               07  RAZAO-ON-SCR-IN             LINE 07 COL 30
                            PIC x(40)        TO ws-ds-rz-social-scr.
           05  LATITUDE-SECTION.
               07  VALUE "Latitude"            LINE 08 COL 10.
               07  LATITUDE-ON-SCR-IN             LINE 07 COL 30
                            PIC s9(03)v9(08) TO ws-nr-latitude-scr.
           05  LONGITUDE-SECTION.
               07  VALUE "Longitude"           LINE 09 COL 10.
               07  LONGITUDE-ON-SCR-IN         LINE 07 COL 30
                            PIC s9(03)v9(08) TO ws-nr-longitude-scr.

      * LINKAGE SECTION.
      *     01 PARAMETRES.
      *         03 PA-RETURN-CODE PIC 99 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE SECTION.
           OPEN EXTEND arq-clientes.
           OPEN I-O arq-clientes.
           PERFORM B-100-LOOP-MENU UNTIL OPCAO-ON-SCR-IN = "9".
           PERFORM B-999-TERMINAR.

       B-100-LOOP-MENU SECTION.
           DISPLAY DADOS-SECTION.
           DISPLAY MENU-SECTION.
           ACCEPT OPCAO-ON-SCR-IN.
           EVALUATE ws-id-opcao
               WHEN "1"
                   PERFORM INCLUIR
               WHEN "2"
                   PERFORM ALTERAR
               WHEN "3"
                   PERFORM EXCLUIR
               WHEN "4"
                   PERFORM IMPORTAR
               WHEN "9"
                   EXIT SECTION
               WHEN OTHER
                   DISPLAY "Opcao Invalida!"
           END-EVALUATE.

       INCLUIR SECTION.
           DISPLAY CNPJ-SECTION.
           ACCEPT CNPJ-ON-SCR-IN.
           DISPLAY RAZAO-SECTION.
           ACCEPT RAZAO-ON-SCR-IN.
           DISPLAY LATITUDE-SECTION.
           ACCEPT LATITUDE-ON-SCR-IN.
           DISPLAY LONGITUDE-SECTION.
           ACCEPT LONGITUDE-ON-SCR-IN.

       ALTERAR SECTION.


       EXCLUIR SECTION.


       IMPORTAR SECTION.

      *     MOVE 0 TO PA-RETURN-CODE
       B-999-TERMINAR SECTION.
           CLOSE arq-clientes
           EXIT PROGRAM.
       END PROGRAM CADASTRO-CLIENTES.
