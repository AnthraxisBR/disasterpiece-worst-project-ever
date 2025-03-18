       IDENTIFICATION DIVISION.
       PROGRAM-ID. SimpleServer.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.


       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RESPONSE-FILE ASSIGN TO "response.tmp"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD RESPONSE-FILE.
       01 RESPONSE-RECORD PIC X(512). *> Define file record structure

       WORKING-STORAGE SECTION.
            COPY VARIABLES.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.

       OPEN-SOCKET.
           CALL "htons" USING BY VALUE PORT RETURNING PORT.

           *> Create socket with SOCK_STREAM (TCP)
           CALL "socket" USING BY VALUE 2, 1, 0 RETURNING SERVER-FD.
           IF SERVER-FD < 0 THEN
               DISPLAY "Error: Could not create socket."
               STOP RUN
           ELSE
               DISPLAY "Socket created successfully."
           END-IF.

       SET-SOCKET-OPTION.
           CALL "setsockopt" USING BY VALUE SERVER-FD
                                   BY VALUE 1      *> SOL_SOCKET
                                   BY VALUE 2      *> SO_REUSEADDR
                                   BY REFERENCE 1  *> Enable option
                                   BY VALUE 4      *> Option size
                                   RETURNING RETURN-CODE.

           DISPLAY "DEBUG: setsockopt RETURN CODE: " RETURN-CODE.


       BIND-SOCKET.
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
           END-IF.

       LISTEN-SOCKET.
           *> Listen for incoming connections
           CALL "listen" USING BY VALUE SERVER-FD, BY VALUE 5 RETURNING RETURN-CODE-LOCAL.
           IF RETURN-CODE-LOCAL < 0 THEN
               DISPLAY "Error: Listen failed."
               STOP RUN
           ELSE
               DISPLAY "Socket is listening."
           END-IF.

           DISPLAY "Waiting for client connection...".
           *> DISPLAY "DEBUG: SERVER-FD: " SERVER-FD
           *> DISPLAY "DEBUG: CLIENT-FD: " CLIENT-FD.

       ACCEPT-CLIENT.
           CALL "accept" USING BY VALUE SERVER-FD,
                                           BY REFERENCE CLIENT-ADDRESS,
                                           BY REFERENCE LENGTH OF CLIENT-ADDRESS
                                           RETURNING CLIENT-FD.

           IF CLIENT-FD < 0 THEN
               DISPLAY "Error: Accept failed."
               STOP RUN
           ELSE
               DISPLAY "Client connection accepted with CLIENT-FD: " CLIENT-FD
           END-IF.

           RECEIVE-REQUEST.

       RECEIVE-REQUEST.
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

           PERFORM PROCESS-REQUEST

           PERFORM SEND-RESPONSE.

       SEND-RESPONSE.
           DISPLAY "Response: " RESPONSE
           DISPLAY "Response size: " LENGTH OF RESPONSE

           MOVE LENGTH OF RESPONSE-BODY TO RESPONSE-BODY-SIZE
           MOVE LENGTH OF RESPONSE TO RESPONSE-SIZE
           PERFORM VARYING I FROM 128 BY -1 UNTIL I = 1
               IF RESPONSE(I:1) NOT = " "
                    MOVE I TO RESPONSE-SIZE
                    EXIT PERFORM
               END-IF
           END-PERFORM

           COMPUTE RESPONSE-SIZE = RESPONSE-SIZE + 6 *> Add 6 for "0000" in "Content-Length: 0000"
           MOVE RESPONSE-SIZE TO RESPONSE-SIZE-NUM
           MOVE RESPONSE-SIZE-NUM TO RESPONSE-SIZE-STRING

           *> DISPLAY "DEBUG: RESPONSE-SIZE: " RESPONSE-SIZE

           MOVE RESPONSE-SIZE-STRING TO RESPONSE(34:4). *> Overwrite "0000" in "Content-Length: 0000"

           CALL "send" USING BY VALUE CLIENT-FD
                               BY REFERENCE RESPONSE
                               BY VALUE LENGTH OF RESPONSE
                               BY VALUE 0
                               RETURNING RETURN-CODE-LOCAL.

           *> DISPLAY "DEBUG: send RETURN CODE: " RETURN-CODE-LOCAL.

           IF RETURN-CODE-LOCAL > 0 THEN
               DISPLAY "Bytes sent: " RETURN-CODE-LOCAL
           ELSE
               DISPLAY "Error: Send failed"
               DISPLAY "Error Code: " RETURN-CODE-LOCAL
           END-IF.

       CLOSE-CONNECTION.
           DISPLAY "Closing connection..."

           CALL "usleep" USING BY VALUE 100000. *> 100ms

           CALL "close" USING BY VALUE CLIENT-FD RETURNING RETURN-CODE-LOCAL.
           *> DISPLAY "DEBUG: close RETURN CODE: " RETURN-CODE-LOCAL.

           *> DISPLAY "Response sent: " RESPONSE.

           GO TO ACCEPT-CLIENT.

       COPY PROCESS-REQUEST.

       STOP RUN.
