       IDENTIFICATION DIVISION.
       PROGRAM-ID. SimpleServer.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CALL-CONVENTION 1 IS C-FUNCTIONS.


       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 SOCKET-FD PIC 9(1) COMP-5.
       01 CLIENT-FD PIC 9(1) COMP-5.
       01 SERVER-FD PIC 9(1) COMP-5.


       01 CLIENT-ADDRESS.
          05 FAMILY          PIC 9(4) COMP-5 VALUE 2.           *> sin_family (AF_INET)
          05 PORT            PIC 9(4) COMP-5 VALUE 8080.        *> sin_port (network byte order)
          05 IP-ADDRESS      PIC X(4) VALUE X"00000000".        *> sin_addr (4-byte IP address)
          05 RESERVED        PIC X(8) VALUE LOW-VALUES.         *> sin_zero (padding)

       01 REQUEST-BUFFER PIC X(1024).
       01 REQUEST-SIZE PIC 9(9) COMP-5.
       01 RESPONSE PIC X(128) VALUE
          "HTTP/1.1 200 OK" & X"0D0A" &
          "Content-Length: 2" & X"0D0A" &
          "Connection: close" & X"0D0A" &
          X"0D0A" & "OK".

       01 RESPONSE-SIZE PIC 9(9) COMP-5 VALUE 52.

       01 RETURN-CODE-LOCAL PIC S9(9) COMP-5.

       PROCEDURE DIVISION.

           CALL "htons" USING BY VALUE PORT RETURNING PORT.

           *> Create socket with SOCK_STREAM (TCP)
           CALL "socket" USING BY VALUE 2, 1, 0 RETURNING SERVER-FD.
           IF SERVER-FD < 0 THEN
               DISPLAY "Error: Could not create socket."
               STOP RUN
           ELSE
               DISPLAY "Socket created successfully."
           END-IF

           *> Bind the socket
           CALL "bind" USING BY VALUE SERVER-FD,
                                      BY CONTENT CLIENT-ADDRESS,
                                      BY VALUE LENGTH OF CLIENT-ADDRESS
                                      RETURNING RETURN-CODE-LOCAL.
           IF RETURN-CODE-LOCAL < 0 THEN
               DISPLAY "Error: Binding socket failed."
               STOP RUN
           ELSE
               DISPLAY "Socket bound successfully."
           END-IF

           *> Listen for incoming connections
           CALL "listen" USING BY VALUE SERVER-FD, BY VALUE 5 RETURNING RETURN-CODE-LOCAL.
           IF RETURN-CODE-LOCAL < 0 THEN
               DISPLAY "Error: Listen failed."
               STOP RUN
           ELSE
               DISPLAY "Socket is listening."
           END-IF

           DISPLAY "Waiting for client connection..."
           DISPLAY "DEBUG: SERVER-FD: " SERVER-FD
           DISPLAY "DEBUG: CLIENT-FD: " CLIENT-FD

           CALL "accept" USING BY VALUE SERVER-FD,
                                           BY REFERENCE CLIENT-ADDRESS,
                                           BY REFERENCE LENGTH OF CLIENT-ADDRESS
                                           RETURNING CLIENT-FD.

           IF CLIENT-FD < 0 THEN
               DISPLAY "Error: Accept failed."
               STOP RUN
           ELSE
               DISPLAY "Client connection accepted with CLIENT-FD: " CLIENT-FD
           END-IF

           CALL "recv" USING BY VALUE CLIENT-FD,
                                    BY REFERENCE REQUEST-BUFFER,
                                    BY VALUE LENGTH OF REQUEST-BUFFER,
                                    BY VALUE 0
                                    RETURNING REQUEST-SIZE.

           IF REQUEST-SIZE < 0 THEN
               DISPLAY "Error: recv failed."
               STOP RUN
           ELSE
               DISPLAY "Received " REQUEST-SIZE " bytes from client."
               DISPLAY "Client Message: " REQUEST-BUFFER
           END-IF.

           SEND-RESPONSE.

       SEND-RESPONSE.
           DISPLAY "Response: " RESPONSE
           DISPLAY "Response size: " LENGTH OF RESPONSE

           CALL "send" USING BY VALUE CLIENT-FD
                               BY REFERENCE RESPONSE
                               BY VALUE 66
                               BY VALUE 0
                               RETURNING RETURN-CODE-LOCAL.

           DISPLAY "DEBUG: send RETURN CODE: " RETURN-CODE-LOCAL.

           *>IF RETURN-CODE-LOCAL < 0 THEN
           *>    DISPLAY "ERROR: send failed with code: " RETURN-CODE-LOCAL.
           *>    CALL "perror" USING BY REFERENCE "send".
           *>    STOP RUN.

           IF RETURN-CODE-LOCAL > 0 THEN
               DISPLAY "Bytes sent: " RETURN-CODE-LOCAL
           ELSE
               DISPLAY "Error: Send failed"
               DISPLAY "Error Code: " RETURN-CODE-LOCAL
           END-IF.

           DISPLAY "Closing connection..."

           CALL "usleep" USING BY VALUE 100000. *> 100ms

           CALL "close" USING BY VALUE CLIENT-FD RETURNING RETURN-CODE-LOCAL.
           DISPLAY "DEBUG: close RETURN CODE: " RETURN-CODE-LOCAL.


           DISPLAY "Response sent: " RESPONSE.


       STOP RUN.
