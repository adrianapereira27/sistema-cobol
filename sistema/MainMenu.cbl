      ******************************************************************
      * Author: Adriana Pereira
      * Date: 22/06/2019
      * Purpose: Criar o menu do sistema
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN-MENU.
       DATA DIVISION.
           FILE SECTION.

       WORKING-STORAGE SECTION.
       01  CAMPOS-SCREEN.
           05  MENU-IN-WS          PIC x(01)  VALUE SPACES.
           05  SUBMENU-IN-WS       PIC x(01)  VALUE SPACES.

       SCREEN SECTION.
       01  DATA-ENTRY-SCREEN.
           05  SISTEMA-SECTION.
               07  VALUE "SISTEMA AMBEV"       BLANK SCREEN
                                               LINE 02 COL 30.
           05  MENU-SECTION.
               07  VALUE "MENU: "              LINE 05 COL 05.
               07  VALUE "1 - CADASTROS"       LINE 07 COL 10.
               07  VALUE "2 - RELATORIOS"      LINE 08 COL 10.
               07  VALUE "3 - EXECUTAR"        LINE 09 COL 10.
               07  VALUE "9 - SAIR"            LINE 10 COL 10.
               07  VALUE "ESCOLHA A OPCAO: "   LINE 12 COL 05.
               07  MENU-ON-SCR-IN              LINE 12 COL 30
                       PIC x(01)               TO MENU-IN-WS.
           05  SUBMENU-SECTION.
               07  VALUE "SUBMENU: "           BLANK SCREEN
                                               LINE 05 COL 05.
           05  SUBMENU-CADASTROS-SECTION.
               07  VALUE "1 - CADASTRO DE CLIENTES"
                                               LINE 07 COL 10.
               07  VALUE "2 - CADASTRO DE VENDEDORES"
                                               LINE 08 COL 10.
           05  SUBMENU-RELATORIOS-SECTION.
               07  VALUE "1 - RELATORIO DE CLIENTES"
                                               LINE 07 COL 10.
               07  VALUE "2 - RELATORIO DE VENDEDORES"
                                               LINE 08 COL 10.
           05  SUBMENU-EXECUTAR-SECTION.
               07  VALUE "1 - EXECUTAR DISTRIBUICAO DE CLIENTES"
                                               LINE 07 COL 10.
           05  SUBMENU-OPCAO-SECTION.
               07  VALUE "9 - SAIR"            LINE 09 COL 10.
               07  VALUE "ESCOLHA A OPCAO: "   LINE 11 COL 05.
               07  SUBMENU-ON-SCR-IN           LINE 11 COL 30
                       PIC x(01)               TO SUBMENU-IN-WS.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE SECTION.
           PERFORM B-100-LOOP-MENU UNTIL MENU-ON-SCR-IN = "9".
           PERFORM B-999-TERMINAR.

       B-100-LOOP-MENU SECTION.
           DISPLAY SISTEMA-SECTION.
           DISPLAY MENU-SECTION.
           ACCEPT MENU-ON-SCR-IN.
           EVALUATE MENU-IN-WS
               WHEN "1"
               WHEN "2"
               WHEN "3"
                   PERFORM B-200-LOOP-SUBMENU
                           UNTIL SUBMENU-ON-SCR-IN = "9"
               WHEN "9"
                   EXIT SECTION
               WHEN OTHER
                   DISPLAY "Opcao Invalida!"
           END-EVALUATE.
           MOVE SPACES TO MENU-ON-SCR-IN.

       B-200-LOOP-SUBMENU SECTION.
           DISPLAY SISTEMA-SECTION.
           DISPLAY SUBMENU-SECTION.
           EVALUATE MENU-IN-WS
               WHEN "1"
                   DISPLAY SUBMENU-CADASTROS-SECTION
               WHEN "2"
                   DISPLAY SUBMENU-RELATORIOS-SECTION
               WHEN "3"
                   DISPLAY SUBMENU-EXECUTAR-SECTION
               WHEN "9"
                   EXIT SECTION
               WHEN OTHER
                   DISPLAY "Opcao Invalida!"
           END-EVALUATE.
           DISPLAY SUBMENU-OPCAO-SECTION.
           ACCEPT SUBMENU-ON-SCR-IN.
           EVALUATE SUBMENU-IN-WS
               WHEN "1"
               WHEN "2"
               WHEN "3"
                   PERFORM B-300-CHAMA-PROGRAMAS
               WHEN "9"
                   EXIT SECTION
               WHEN OTHER
                   DISPLAY "Opcao Invalida!"
           END-EVALUATE.
           MOVE SPACES TO SUBMENU-ON-SCR-IN.

       B-300-CHAMA-PROGRAMAS SECTION.
           IF  MENU-IN-WS = "1"
               IF  SUBMENU-IN-WS = "1"
                   CALL "CADASTRO-CLIENTES"
                          USING BY CONTENT SUBMENU-IN-WS
                   CANCEL "CADASTRO-CLIENTES"
               ELSE
                   CALL "CADASTRO-VENDEDORES"
                          USING BY CONTENT SUBMENU-IN-WS
                   CANCEL "CADASTRO-VENDEDORES"
               END-IF
           ELSE
               IF  MENU-IN-WS = "2"
                   IF  SUBMENU-IN-WS = "1"
                       CALL "RELATORIO-CLIENTES"
                             USING BY CONTENT SUBMENU-IN-WS
                       CANCEL "RELATORIO-CLIENTES"
                   ELSE
                       CALL "RELATORIO-CLIENTES"
                             USING BY CONTENT SUBMENU-IN-WS
                       CANCEL "RELATORIO-CLIENTES"
                   END-IF
               ELSE
                   CALL "CADASTRO-CLIENTES"
                         USING BY CONTENT SUBMENU-IN-WS
                   CANCEL "CADASTRO-CLIENTES"
               END-IF
           END-IF.

       B-999-TERMINAR SECTION.
            STOP RUN.
       END PROGRAM MAIN-MENU.
