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
             RECORD KEY         IS rw-nr-cnpj
             ALTERNATE KEY      IS rw-cd-cliente
             LOCK MODE          IS MANUAL
             FILE STATUS        IS ws-resultado-acesso.

           SELECT arquivo-import-csv ASSIGN TO arquivocsv
                  ORGANIZATION  IS LINE SEQUENTIAL
                  ACCESS MODE   IS SEQUENTIAL
                  FILE STATUS   IS ws-resultado-acesso.

           SELECT arquivo-log   ASSIGN TO "arqlog.txt"
                  ORGANIZATION  IS LINE SEQUENTIAL
                  ACCESS MODE   IS SEQUENTIAL
                  FILE STATUS   IS ws-resultado-acesso.

       DATA DIVISION.
       FILE SECTION.
           FD arq-clientes.
           01  rw-registro.
               03 rw-nr-cnpj               PIC 9(014).
               03 rw-cd-cliente            PIC 9(007).
               03 rw-ds-razao-social       PIC x(040).
               03 rw-nr-latitude           PIC s9(003)v9(008).
               03 rw-nr-longitude          PIC s9(003)v9(008).

           FD arquivo-import-csv.
           01  rw-registro-csv.
               03 rw-ds-registro-csv       PIC x(300).

           FD arquivo-log.
           01  rw-registro-log.
               03 rw-ds-registro-log       PIC x(200).

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
               03 ws-ds-arquivo            PIC x(60) VALUE SPACES.
               03 ws-ds-caminho            PIC x(150) VALUE SPACES.
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
               07  VALUE "CNPJ:"               LINE 06 COL 05.
               07  CNPJ-ON-SCR-IN              LINE 06 COL 20
                            PIC 9(14)        TO rw-nr-cnpj.
           05  RAZAO-SECTION.
               07  VALUE "Razao social:"       LINE 07 COL 05.
               07  RAZAO-ON-SCR-IN             LINE 07 COL 20
                            PIC x(40)        TO rw-ds-razao-social.
           05  LATITUDE-SECTION.
               07  VALUE "Latitude:"           LINE 08 COL 05.
               07  LATITUDE-ON-SCR-IN          LINE 08 COL 20
                            PIC s9(03)v9(08) TO rw-nr-latitude.
           05  LONGITUDE-SECTION.
               07  VALUE "Longitude:"          LINE 09 COL 05.
               07  LONGITUDE-ON-SCR-IN         LINE 09 COL 20
                            PIC s9(03)v9(08) TO rw-nr-longitude.
           05  ARQUIVO-IMPORT-SECTION.
               07  VALUE "Nome do arquivo:"    LINE 06 COL 05.
               07  CAMINHO-ARQ-ON-SCR-IN       LINE 06 COL 24
                            PIC x(50)        TO ws-ds-arquivo.

      * LINKAGE SECTION.
      *     01 PARAMETRES.
      *         03 PA-RETURN-CODE PIC 99 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE SECTION.
      *     SET ENVIRONMENT 'COB_SCREEN_ESC' TO 'Y'.
           OPEN I-O arq-clientes.
           IF  NOT ws-operacao-ok
               OPEN OUTPUT arq-clientes
               CLOSE arq-clientes
               OPEN I-O arq-clientes
           END-IF.

           COPY ValidaCpfCnpj.CPY.

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
           MOVE SPACES TO OPCAO-ON-SCR-IN.

       INCLUIR SECTION.
           DISPLAY DADOS-SECTION.
           DISPLAY CNPJ-SECTION.
           ACCEPT CNPJ-ON-SCR-IN.
           DISPLAY RAZAO-SECTION.
           ACCEPT RAZAO-ON-SCR-IN.
           DISPLAY LATITUDE-SECTION.
           ACCEPT LATITUDE-ON-SCR-IN.
           DISPLAY LONGITUDE-SECTION.
           ACCEPT LONGITUDE-ON-SCR-IN.

           WRITE rw-registro.
           IF  ws-operacao-ok
               DISPLAY "Gravado com sucesso"
           ELSE
               DISPLAY "Erro ao gravar dados"
           END-IF
           PERFORM B-100-LOOP-MENU.

       ALTERAR SECTION.
           DISPLAY DADOS-SECTION.
           DISPLAY CNPJ-SECTION.
           ACCEPT CNPJ-ON-SCR-IN.

           START arq-clientes KEY IS EQUAL rw-nr-cnpj
              INVALID KEY DISPLAY "CNPJ Invalido"
                   PERFORM ALTERAR
              NOT INVALID KEY
                   PERFORM REGRAVA-DADOS
           END-START.

       REGRAVA-DADOS SECTION.
           DISPLAY RAZAO-SECTION.
           ACCEPT RAZAO-ON-SCR-IN.
           DISPLAY LATITUDE-SECTION.
           ACCEPT LATITUDE-ON-SCR-IN.
           DISPLAY LONGITUDE-SECTION.
           ACCEPT LONGITUDE-ON-SCR-IN.

           REWRITE rw-registro.
           IF  ws-operacao-ok
               DISPLAY "Regravado com sucesso"
           ELSE
               DISPLAY "Erro ao regravar dados"
           END-IF
           PERFORM B-100-LOOP-MENU.

       EXCLUIR SECTION.
           DISPLAY DADOS-SECTION.
           DISPLAY CNPJ-SECTION.
           ACCEPT CNPJ-ON-SCR-IN.

           START arq-clientes KEY IS EQUAL rw-nr-cnpj
              INVALID KEY DISPLAY "CNPJ Invalido"
                   PERFORM EXCLUIR
              NOT INVALID KEY
                   PERFORM EXCLUI-DADOS
           END-START.

       EXCLUI-DADOS SECTION.
           DELETE arq-clientes.
           IF  ws-operacao-ok
               DISPLAY "Excluido com sucesso"
           ELSE
               DISPLAY "Erro ao excluir dados"
           END-IF
           PERFORM B-100-LOOP-MENU.

       IMPORTAR SECTION.
           DISPLAY DADOS-SECTION.
           DISPLAY ARQUIVO-IMPORT-SECTION.
           ACCEPT CAMINHO-ARQ-ON-SCR-IN.

           STRING FUNCTION MODULE-PATH DELIMITED BY " " ws-ds-arquivo
                                       INTO ws-ds-caminho


           .
      *     MOVE 0 TO PA-RETURN-CODE
       B-999-TERMINAR SECTION.
           CLOSE arq-clientes
           CLOSE arquivo-import-csv
           EXIT PROGRAM.
       END PROGRAM CADASTRO-CLIENTES.
