
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.

       DATA             DIVISION.

       SCREEN           SECTION.
       01  scr.
           03  VALUE "foo1" BLANK SCREEN.
       01  scr2.
           03  VALUE "foo2" BLANK.
       01  scr3.
           03  VALUE "foo3" ERASE EOS.
       01  scr4.
           03  VALUE "foo4" ERASE.

       PROCEDURE        DIVISION.
           DISPLAY "one" BLANK SCREEN.
           DISPLAY "two" BLANK.
           DISPLAY " 3 " ERASE TO END OF SCREEN.
           DISPLAY " 4 " ERASE
           .
