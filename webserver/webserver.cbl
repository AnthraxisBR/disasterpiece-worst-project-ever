       IDENTIFICATION DIVISION.
       PROGRAM-ID. Server.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CALL-CONVENTION 1 IS C-FUNCTIONS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 SOCKET-FD PIC 9(9) COMP-5.
       01 CLIENT-FD PIC 9(9) COMP-5.
       01 SERVER-FD PIC 9(9) COMP-5.

       01 SERVER-ADDRESS.
           05 FAMILY          PIC 9(4) COMP-5 VALUE 2. *> AF_INET (IPv4)
           05 PORT            PIC 9(4) COMP-5 VALUE 8080.
           05 IP-ADDRESS      PIC X(4) VALUE X"00000000". *> 0.0.0.0
           05 RESERVED        PIC X(8) VALUE LOW-VALUES.

       01 CLIENT-ADDRESS PIC X(16).
       01 CLIENT-ADDR-LEN PIC 9(4) COMP-5 VALUE 16.

       01 REQUEST-BUFFER PIC X(1024).
       01 REQUEST-SIZE PIC 9(9) COMP-5.

       01 RESPONSE-SIZE PIC 9(9) COMP-5 VALUE 52.
       01 RESPONSE-SIZE-STRING PIC X(2). *> Store ASCII version of RESPONSE-SIZE
       01 RESPONSE-SIZE-NUM PIC 9(2). *> Numeric version without COMP-5

       01 OPT-VALUE PIC S9(9) COMP-5 VALUE 1.

       01 RESPONSE PIC X(128) VALUE "HTTP/1.1 200 OK" & X"0D0A" & "Content-Length: 2" & X"0D0A" & "Connection: close" & X"0D0A" & "OK".
       01 RESPONSE-BODY PIC X(128) VALUE  X"0D0A" & "OK".

       01 RETURN-CODE-LOCAL PIC S9(9) COMP-5.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           CALL "htons" USING BY VALUE PORT RETURNING PORT.

           CALL "socket" USING BY VALUE 2, 1, 0 RETURNING SERVER-FD.
           IF SERVER-FD < 0 THEN
               DISPLAY "Error: Could not create socket"
               STOP RUN
           END-IF


           CALL "setsockopt" USING BY VALUE SERVER-FD,
                          BY VALUE 1, *> SOL_SOCKET
                          BY VALUE 2, *> SO_REUSEADDR
                          BY REFERENCE OPT-VALUE,
                          BY VALUE LENGTH OF OPT-VALUE
                          RETURNING RETURN-CODE-LOCAL.
           DISPLAY "DEBUG: setsockopt RETURN CODE: " RETURN-CODE-LOCAL.
           IF RETURN-CODE-LOCAL < 0 THEN
               DISPLAY "Error: setsockopt failed"
               STOP RUN
           END-IF

           CALL "fcntl" USING BY VALUE CLIENT-FD,
                             BY VALUE 4,         *> F_SETFL
                             BY VALUE 2048,      *> O_NONBLOCK
                             RETURNING RETURN-CODE-LOCAL.

           DISPLAY "DEBUG: fcntl RETURN CODE: " RETURN-CODE-LOCAL.

           CALL "bind" USING BY VALUE SERVER-FD,
                                  BY CONTENT SERVER-ADDRESS,
                                  BY VALUE LENGTH OF SERVER-ADDRESS
                                  RETURNING RETURN-CODE-LOCAL.
           IF RETURN-CODE-LOCAL < 0 THEN
               DISPLAY "Error: Bind failed"
               STOP RUN
           END-IF

           CALL "listen" USING BY VALUE SERVER-FD, BY VALUE 5 RETURNING RETURN-CODE-LOCAL.
           IF RETURN-CODE-LOCAL < 0 THEN
               DISPLAY "Error: Listen failed"
               STOP RUN
           END-IF


           DISPLAY "COBOL Server listening on port 8080".

           PERFORM ACCEPT-CLIENT.

       ACCEPT-CLIENT.
           CALL "accept" USING BY VALUE SERVER-FD,
                                       BY REFERENCE CLIENT-ADDRESS,
                                       BY VALUE CLIENT-ADDR-LEN
                                       RETURNING CLIENT-FD.
           IF CLIENT-FD < 0 THEN
               DISPLAY "Error: Accept failed"
               STOP RUN
           END-IF


           CALL "recv" USING BY VALUE CLIENT-FD,
                                BY REFERENCE REQUEST-BUFFER,
                                BY VALUE LENGTH OF REQUEST-BUFFER,
                                BY VALUE 0
                                RETURNING REQUEST-SIZE.

           DISPLAY "Received Request: " REQUEST-BUFFER.

           INSPECT RESPONSE TALLYING RESPONSE-SIZE FOR CHARACTERS

           MOVE RESPONSE-SIZE TO RESPONSE-SIZE-NUM.
           MOVE RESPONSE-SIZE-NUM TO RESPONSE-SIZE-STRING. *> MOVE RESPONSE-SIZE-STRING TO RESPONSE(34:4). *> Overwrite "00" in "Content-Length: 00"

           PERFORM SEND-RESPONSE.

           CALL "usleep" USING BY VALUE 500000.
           CALL "close" USING BY VALUE CLIENT-FD.

           GO TO ACCEPT-CLIENT.

       SEND-RESPONSE.
           DISPLAY "Response: " RESPONSE
           DISPLAY "Response size: " RESPONSE-SIZE

           CALL "write" USING BY VALUE CLIENT-FD
                               BY REFERENCE RESPONSE
                               BY VALUE RESPONSE-SIZE
                               RETURNING RETURN-CODE-LOCAL

           DISPLAY "DEBUG: send RETURN CODE: " RETURN-CODE-LOCAL

           IF RETURN-CODE-LOCAL > 0 THEN
               DISPLAY "Bytes sent: " RETURN-CODE-LOCAL
           ELSE
               DISPLAY "Error: Send failed"
           END-IF

           CALL "usleep" USING BY VALUE 500000.  *> Sleep for 500 milliseconds


           CALL "shutdown" USING BY VALUE CLIENT-FD,
                                       BY VALUE 2         *> SHUT_RDWR
                                       RETURNING RETURN-CODE-LOCAL

           CALL "close" USING BY VALUE CLIENT-FD

           DISPLAY "Response sent: " RESPONSE.

           GO TO ACCEPT-CLIENT.


       STOP RUN.
