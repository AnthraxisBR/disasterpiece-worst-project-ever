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

       01 CLIENT-ADDRESS.
           05 CLIENT-FAMILY    PIC 9(4) COMP-5.         *> sin_family (2 bytes) (AF_INET)
           05 CLIENT-PORT      PIC 9(4) COMP-5.         *> sin_port (2 bytes)
           05 CLIENT-IP        PIC X(4).                *> sin_addr.s_addr (4 bytes)
           05 CLIENT-ZERO      PIC X(8).                *> Padding (sin_zero, 8 bytes)

       01 CLIENT-ADDR-LEN PIC 9(4) COMP-5 VALUE 16.      *> Length of sockaddr_in (16 bytes)

       01 REQUEST-BUFFER PIC X(1024).
       01 REQUEST-SIZE PIC 9(9) COMP-5.

       01 TOTAL-SIZE PIC 9(5) VALUE 0.
       01 CHUNK-SIZE PIC 9(4) VALUE 512.
       01 CHUNK-BUFFER PIC X(2048).
       01 RECEIVED-SIZE PIC 9(5) VALUE 0.

       01 RESPONSE-SIZE PIC 9(9) COMP-5 VALUE 52.
       01 RESPONSE-SIZE-STRING PIC X(2). *> Store ASCII version of RESPONSE-SIZE
       01 RESPONSE-SIZE-NUM PIC 9(2). *> Numeric version without COMP-5

       01 OPT-VALUE PIC S9(9) COMP-5 VALUE 1.

       01 RESPONSE PIC X(128) VALUE "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nOK".
       01 RESPONSE-BODY PIC X(128) VALUE  X"0D0A" & "OK".

       01 RETURN-CODE-LOCAL PIC S9(9) COMP-5.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           CALL "htons" USING BY VALUE PORT RETURNING PORT.

           CALL "socket" USING BY VALUE 2, 1, 0 RETURNING SERVER-FD.
           DISPLAY "DEBUG: SERVER-FD: " SERVER-FD.
           IF SERVER-FD < 0 THEN
               DISPLAY "Error: Could not create socket."
               STOP RUN
           ELSE
               DISPLAY "Socket created successfully."
           END-IF


           *> CALL "setsockopt" USING BY VALUE SERVER-FD,
           *>                BY VALUE 1, *> SOL_SOCKET
           *>                BY VALUE 2, *> SO_REUSEADDR
           *>                BY REFERENCE OPT-VALUE,
           *>                BY VALUE LENGTH OF OPT-VALUE
           *>                RETURNING RETURN-CODE-LOCAL.

           DISPLAY "DEBUG: setsockopt RETURN CODE: " RETURN-CODE-LOCAL.
           IF RETURN-CODE-LOCAL < 0 THEN
               DISPLAY "Error: setsockopt failed"
               STOP RUN
           END-IF


           CALL "bind" USING BY VALUE SERVER-FD,
                                  BY CONTENT SERVER-ADDRESS,
                                  BY VALUE LENGTH OF SERVER-ADDRESS
                                  RETURNING RETURN-CODE-LOCAL.
           IF RETURN-CODE-LOCAL < 0 THEN
               DISPLAY "Error: Binding socket failed."
               STOP RUN
           ELSE
               DISPLAY "Socket bound successfully."
           END-IF

           CALL "listen" USING BY VALUE SERVER-FD, BY VALUE 5 RETURNING RETURN-CODE-LOCAL.
           IF RETURN-CODE-LOCAL < 0 THEN
               DISPLAY "Error: Listen failed."
               STOP RUN
           ELSE
               DISPLAY "Socket is listening."
           END-IF

           DISPLAY "COBOL Server listening on port 8080".

           *> PERFORM ACCEPT-CLIENT.

           *> ACCEPT-CLIENT.
           DISPLAY "Waiting for client connection..."
           DISPLAY "DEBUG: SERVER-FD: " SERVER-FD.

           CALL "accept" USING BY VALUE SERVER-FD,
                                       BY REFERENCE CLIENT-ADDRESS,
                                       BY VALUE CLIENT-ADDR-LEN
                                       RETURNING CLIENT-FD.

           DISPLAY "DEBUG: CLIENT-FD after accept: " CLIENT-FD.

           IF CLIENT-FD < 0 THEN
               DISPLAY "Error: Accept failed."
               STOP RUN
           ELSE
               DISPLAY "Client connection accepted."
           END-IF

           MOVE 0 TO TOTAL-SIZE.
           MOVE 0 TO RECEIVED-SIZE.
           DISPLAY "DEBUG: SERVER-FD: " SERVER-FD.
           PERFORM UNTIL RECEIVED-SIZE > 0
               DISPLAY "Entering loop, RECEIVED-SIZE: " RECEIVED-SIZE
               DISPLAY "Waiting for data..."
               *> display the socker status
               DISPLAY "DEBUG: CLIENT-FD: " CLIENT-FD
               CALL "recv" USING BY REFERENCE CLIENT-FD,
                                BY REFERENCE CHUNK-BUFFER,
                                BY VALUE CHUNK-SIZE,
                                BY VALUE 0
                                RETURNING RECEIVED-SIZE

               IF RECEIVED-SIZE > 0 THEN
                   STRING CHUNK-BUFFER DELIMITED BY SIZE
                          INTO REQUEST-BUFFER WITH POINTER TOTAL-SIZE
                   ADD RECEIVED-SIZE TO TOTAL-SIZE
                   DISPLAY "Received " RECEIVED-SIZE " bytes of data."
                   DISPLAY "Data: " CHUNK-BUFFER
               ELSE IF RECEIVED-SIZE = 0 THEN
                   DISPLAY "Connection closed by client."
               ELSE
                   DISPLAY "Error occurred while receiving data."
                   CALL "errno" RETURNING RETURN-CODE-LOCAL
                   DISPLAY "Error Code: " RETURN-CODE-LOCAL
               END-IF
           END-PERFORM.

           IF TOTAL-SIZE = 0 THEN
               DISPLAY "No data received. The client might have closed the connection."
           END-IF.

           INSPECT RESPONSE TALLYING RESPONSE-SIZE FOR CHARACTERS

           DISPLAY "Response size: " RESPONSE-SIZE

           MOVE RESPONSE-SIZE TO RESPONSE-SIZE-NUM.
           MOVE RESPONSE-SIZE-NUM TO RESPONSE-SIZE-STRING. *> MOVE RESPONSE-SIZE-STRING TO RESPONSE(34:4). *> Overwrite "00" in "Content-Length: 00"

           DISPLAY "Response size: " RESPONSE-SIZE-STRING

           PERFORM SEND-RESPONSE.

           CALL "usleep" USING BY VALUE 500000.
           CALL "close" USING BY VALUE CLIENT-FD.

           *> GO TO ACCEPT-CLIENT.

       SEND-RESPONSE.
           DISPLAY "Response: " RESPONSE
           DISPLAY "Response size: " RESPONSE-SIZE

           CALL "send" USING BY VALUE CLIENT-FD
                               BY REFERENCE RESPONSE
                               BY VALUE RESPONSE-SIZE
                               BY VALUE 0
                               RETURNING RETURN-CODE-LOCAL

           DISPLAY "DEBUG: send RETURN CODE: " RETURN-CODE-LOCAL


           IF RETURN-CODE-LOCAL > 0 THEN
               DISPLAY "Bytes sent: " RETURN-CODE-LOCAL
           ELSE
               DISPLAY "Error: Send failed"
               DISPLAY "Error Code: " RETURN-CODE-LOCAL
           END-IF

           CALL "usleep" USING BY VALUE 500000.  *> Sleep for 500 milliseconds


           CALL "shutdown" USING BY VALUE CLIENT-FD,
                                       BY VALUE 2         *> SHUT_RDWR
                                       RETURNING RETURN-CODE-LOCAL

           CALL "close" USING BY VALUE CLIENT-FD

           DISPLAY "Response sent: " RESPONSE.

           *> GO TO ACCEPT-CLIENT.


       STOP RUN.
