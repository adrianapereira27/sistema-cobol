
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       ENVIRONMENT      DIVISION.
       INPUT-OUTPUT     SECTION.
       FILE-CONTROL.
       SELECT test-file ASSIGN path
                        ORGANIZATION LINE SEQUENTIAL.
       DATA             DIVISION.
       FILE	            SECTION.
       FD  test-file.
       01  test-rec     PIC X(5).
       WORKING-STORAGE    SECTION.
       01  path         BASED PIC X(10).
       PROCEDURE        DIVISION.
           ALLOCATE path
           MOVE "test.txt" TO path
           OPEN INPUT test-file
           FREE path
           READ test-file   END-READ
           IF test-rec NOT = "hello"
              DISPLAY test-rec END-DISPLAY
           END-IF
           CLOSE test-file
           STOP RUN.
