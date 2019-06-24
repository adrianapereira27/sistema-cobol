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
           01  rw-registro-csv             PIC x(200).

           FD arquivo-log.
           01  rw-registro-log             PIC x(200).

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
               03 ws-nr-linha              PIC 9(06) VALUE ZEROS.
               03 ws-escape-key            PIC 9(04) VALUE ZEROS.
                  88 cob-scr-esc               VALUE 2005.

           01 ws-campos-importacao.
               03 ws-cd-cliente            PIC 9(007).
               03 ws-nr-cnpj               PIC 9(014).
               03 ws-ds-razao-social       PIC x(040).
      *         03 ws-nr-latitude-v         PIC s9(003)v9(008).
               03 ws-nr-latitude           PIC -9(003),9(008).
      *         03 ws-nr-longitude-v        PIC s9(003)v9(008).
               03 ws-nr-longitude          PIC -9(003),9(008).
               03 ws-cd-cliente-str        PIC x(007).
               03 ws-nr-cnpj-str           PIC x(014).
               03 ws-nr-latitude-str       PIC x(013).
               03 ws-nr-longitude-str      PIC x(013).

           01  PARAMETROS-CNPJ.
               05 CODIGO-CNPJ              PIC 9(014) VALUE ZEROS.
               05 CNPJ-RETORNO             PIC X(002) VALUE SPACES.

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
                            PIC -9(03),9(08) TO rw-nr-latitude.
           05  LONGITUDE-SECTION.
               07  VALUE "Longitude:"          LINE 09 COL 05.
               07  LONGITUDE-ON-SCR-IN         LINE 09 COL 20
                            PIC -9(03),9(08) TO rw-nr-longitude.
           05  ARQUIVO-IMPORT-SECTION.
               07  VALUE "Nome do arquivo:"    LINE 06 COL 05.
               07  CAMINHO-ARQ-ON-SCR-IN       LINE 06 COL 24
                            PIC x(50)        TO ws-ds-arquivo.
           05  ESCAPE-SECTION.
               07  VALUE "Pressione a tecla ESC para voltar ao menu"
                             LINE 11 COL 05  USING ws-escape-key.
      *         07  ESCAPE-ON-SCR-IN            LINE 11 COL 35
      *                      PIC 9(02)        TO ws-escape-key.
           05  MENSAGEM-SECTION.
               07  VALUE "Mensagem:"           LINE 15 COL 05
                                             USING whs-mensagem.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE SECTION.
           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'.
           SET ENVIRONMENT 'COB_SCREEN_ESC' TO 'Y'.

           OPEN I-O arq-clientes.
           IF  NOT ws-operacao-ok
               OPEN OUTPUT arq-clientes
               CLOSE arq-clientes
               OPEN I-O arq-clientes
           END-IF.

           PERFORM B-100-LOOP-MENU UNTIL OPCAO-ON-SCR-IN = "9".
           PERFORM B-999-TERMINAR.

       B-100-LOOP-MENU SECTION.
           PERFORM LIMPA-CAMPOS-TELA.
           DISPLAY DADOS-SECTION.
           DISPLAY MENU-SECTION.
           ACCEPT OPCAO-ON-SCR-IN.
           EVALUATE ws-id-opcao
               WHEN "1"
                   PERFORM INCLUIR *> UNTIL NOT cob-scr-esc
               WHEN "2"
                   PERFORM ALTERAR *> UNTIL NOT cob-scr-esc
               WHEN "3"
                   PERFORM EXCLUIR *> UNTIL NOT cob-scr-esc
               WHEN "4"
                   PERFORM IMPORTAR *> UNTIL NOT cob-scr-esc
               WHEN "9"
                   EXIT SECTION
               WHEN OTHER
                   MOVE "Opcao Invalida!" TO whs-mensagem
           END-EVALUATE.
           PERFORM LIMPA-CAMPOS-TELA.
           IF  whs-mensagem NOT EQUAL SPACES
               DISPLAY MENSAGEM-SECTION
               ACCEPT MENSAGEM-SECTION
           END-IF.

       INCLUIR SECTION.
           PERFORM LIMPA-CAMPOS-TELA.
           DISPLAY DADOS-SECTION.
           DISPLAY CNPJ-SECTION.
           ACCEPT CNPJ-ON-SCR-IN.

           MOVE SPACES                   TO CNPJ-RETORNO
           MOVE rw-nr-cnpj               TO CODIGO-CNPJ
           CALL "VALIDAR-CNPJ" USING PARAMETROS-CNPJ
           CANCEL "VALIDAR-CNPJ".
           IF CNPJ-RETORNO NOT EQUAL "00"
              MOVE SPACES                TO whs-mensagem
              STRING "CNPJ invalido!"
                     DELIMITED BY SIZE INTO whs-mensagem
              EXIT SECTION
           END-IF.

           DISPLAY RAZAO-SECTION.
           ACCEPT RAZAO-ON-SCR-IN.
           DISPLAY LATITUDE-SECTION.
           ACCEPT LATITUDE-ON-SCR-IN.
           DISPLAY LONGITUDE-SECTION.
           ACCEPT LONGITUDE-ON-SCR-IN.

           WRITE rw-registro.
           IF  ws-operacao-ok
               MOVE "Gravado com sucesso" TO whs-mensagem
           ELSE
               MOVE "Erro ao gravar dados" TO whs-mensagem
           END-IF.
           DISPLAY ESCAPE-SECTION.
           ACCEPT COB-CRT-STATUS FROM ESCAPE KEY.

       ALTERAR SECTION.
           PERFORM LIMPA-CAMPOS-TELA.
           DISPLAY DADOS-SECTION.
           DISPLAY CNPJ-SECTION.
           ACCEPT CNPJ-ON-SCR-IN.

           START arq-clientes KEY IS EQUAL rw-nr-cnpj
              INVALID KEY
                   MOVE "CNPJ Invalido" TO whs-mensagem
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
               MOVE "Regravado com sucesso" TO whs-mensagem
           ELSE
               MOVE "Erro ao regravar dados" TO whs-mensagem
           END-IF.

       EXCLUIR SECTION.
           PERFORM LIMPA-CAMPOS-TELA.
           DISPLAY DADOS-SECTION.
           DISPLAY CNPJ-SECTION.
           ACCEPT CNPJ-ON-SCR-IN.

           START arq-clientes KEY IS EQUAL rw-nr-cnpj
              INVALID KEY
                   MOVE "CNPJ Invalido" TO whs-mensagem
              NOT INVALID KEY
                   PERFORM EXCLUI-DADOS
           END-START.

       EXCLUI-DADOS SECTION.
           DELETE arq-clientes.
           IF  ws-operacao-ok
               MOVE "Excluido com sucesso" TO whs-mensagem
           ELSE
               MOVE "Erro ao excluir dados" TO whs-mensagem
           END-IF.

       IMPORTAR SECTION.
           PERFORM LIMPA-CAMPOS-TELA.
           DISPLAY DADOS-SECTION.
           DISPLAY ARQUIVO-IMPORT-SECTION.
           ACCEPT CAMINHO-ARQ-ON-SCR-IN.

           OPEN OUTPUT arquivo-log
           STRING FUNCTION MODULE-PATH DELIMITED BY " " ws-ds-arquivo
                                           INTO ws-ds-caminho
           MOVE ws-ds-caminho              TO arquivocsv
           OPEN INPUT arquivo-import-csv
           IF  NOT ws-operacao-ok
               DISPLAY "Arquivo nao encontrado."
               PERFORM B-100-LOOP-MENU
               EXIT SECTION
           END-IF

           READ arquivo-import-csv
           UNSTRING rw-registro-csv DELIMITED BY ";"
                                            INTO ws-cd-cliente-str
                                                 ws-nr-cnpj-str
                                                 ws-ds-razao-social
                                                 ws-nr-latitude-str
                                                 ws-nr-longitude-str
           IF  FUNCTION NUMVAL(ws-cd-cliente-str) EQUAL ZEROS
           AND FUNCTION NUMVAL(ws-nr-cnpj-str) EQUAL ZEROS
               ADD 1                        TO ws-nr-linha
               READ arquivo-import-csv
           END-IF

           PERFORM UNTIL NOT ws-operacao-ok
               ADD 1                        TO ws-nr-linha
               UNSTRING rw-registro-csv DELIMITED BY ";"
                                            INTO ws-cd-cliente-str
                                                 ws-nr-cnpj-str
                                                 ws-ds-razao-social
                                                 ws-nr-latitude-str
                                                 ws-nr-longitude-str

               MOVE FUNCTION NUMVAL(ws-cd-cliente-str) TO ws-cd-cliente
               IF  ws-cd-cliente EQUAL ZEROS
                   MOVE SPACES              TO rw-registro-log
                   STRING "Codigo do cliente invalido na linha "
                          ws-nr-linha INTO rw-registro-log
                   WRITE rw-registro-log
               END-IF
               MOVE FUNCTION NUMVAL(ws-nr-cnpj-str) TO ws-nr-cnpj
               IF  ws-nr-cnpj EQUAL ZEROS
                   MOVE SPACES              TO rw-registro-log
                   STRING "CNPJ invalido na linha "
                          ws-nr-linha INTO rw-registro-log
                   WRITE rw-registro-log
               ELSE
                   MOVE SPACES                TO CNPJ-RETORNO
                   MOVE ws-nr-cnpj            TO CODIGO-CNPJ
                   CALL "VALIDAR-CNPJ" USING PARAMETROS-CNPJ
                   CANCEL "VALIDAR-CNPJ"
                   IF  CNPJ-RETORNO NOT EQUAL "00"
                       MOVE SPACES            TO rw-registro-log
                       STRING "CNPJ invalido na linha "
                              ws-nr-linha INTO rw-registro-log
                       WRITE rw-registro-log
                   END-IF
               END-IF
               MOVE FUNCTION NUMVAL(ws-nr-latitude-str)
                                            TO ws-nr-latitude
               IF  ws-nr-latitude EQUAL ZEROS
                   MOVE SPACES              TO rw-registro-log
                   STRING "Latitude invalida na linha "
                          ws-nr-linha INTO rw-registro-log
                   WRITE rw-registro-log
               END-IF
               MOVE FUNCTION NUMVAL(ws-nr-longitude-str)
                                            TO ws-nr-longitude
               IF  ws-nr-longitude EQUAL ZEROS
                   MOVE SPACES              TO rw-registro-log
                   STRING "Longitude invalida na linha "
                          ws-nr-linha INTO rw-registro-log
                   WRITE rw-registro-log
               END-IF
               IF  rw-registro-log EQUAL SPACES
                   INITIALISE               rw-registro
                   MOVE ws-nr-cnpj          TO rw-nr-cnpj
                   MOVE ws-cd-cliente       TO rw-cd-cliente
                   MOVE ws-ds-razao-social  TO rw-ds-razao-social
                   MOVE ws-nr-latitude      TO rw-nr-latitude
                   MOVE ws-nr-longitude     TO rw-nr-longitude
                   WRITE rw-registro
                   IF  ws-registro-existente
                       MOVE SPACES          TO rw-registro-log
                       STRING "CNPJ da linha " ws-nr-linha
                              " ja existente no sistema"
                                           INTO rw-registro-log
                       WRITE rw-registro-log
                   END-IF
               END-IF
               READ arquivo-import-csv
           END-PERFORM

           CLOSE arquivo-log
           OPEN INPUT arquivo-log
           READ arquivo-log
           IF  ws-operacao-ok
               DISPLAY "Arquivo csv importado com erros"
           ELSE
               DISPLAY "Arquivo csv importado com sucesso"
           END-IF
           .

       LIMPA-CAMPOS-TELA SECTION.
           MOVE SPACES TO OPCAO-ON-SCR-IN.
           MOVE ZEROS TO CNPJ-ON-SCR-IN.
           MOVE SPACES TO RAZAO-ON-SCR-IN.
           MOVE ZEROS TO LATITUDE-ON-SCR-IN.
           MOVE ZEROS TO LONGITUDE-ON-SCR-IN.
           MOVE SPACES TO CAMINHO-ARQ-ON-SCR-IN.

       B-999-TERMINAR SECTION.
           CLOSE arq-clientes
           CLOSE arquivo-import-csv
           CLOSE arquivo-log
           EXIT PROGRAM.
       END PROGRAM CADASTRO-CLIENTES.
