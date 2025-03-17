       PROCESS-REQUEST.
            DISPLAY "Processing request..."
            PERFORM PARSE-INCOMING-REQUEST
            DISPLAY "Request processed."

            DISPLAY "Handling routing..."
            PERFORM HANDLE-ROUTING
            DISPLAY "Routing handled."

            EXIT PARAGRAPH.

       PARSE-INCOMING-REQUEST.
            DISPLAY "Parsing request...".

            PERFORM IDENTIFY-REQUEST-METHOD

            PERFORM VARYING GET-POSITION FROM 1 BY 1 UNTIL GET-POSITION > FUNCTION LENGTH(REQUEST-BUFFER)
                IF REQUEST-BUFFER(GET-POSITION:LENGTH OF HTTP-METHOD) = HTTP-METHOD THEN
                    EXIT PERFORM
                END-IF
            END-PERFORM.

            IF GET-POSITION > 0 THEN
                MOVE REQUEST-BUFFER TO TEMP-REQUEST-LINE

                *> Extract URL between  and " HTTP/1.1"
                UNSTRING TEMP-REQUEST-LINE DELIMITED BY ALL " "
                    INTO DUMMY-VAR URL-PATH DUMMY-VAR

                DISPLAY "DEBUG: Extracted URL Path: " URL-PATH
            ELSE
                DISPLAY "ERROR: Method not found in request: " HTTP-METHOD
            END-IF

            IF HTTP-METHOD = "POST" OR HTTP-METHOD = "PUT" THEN
                MOVE 0 TO BODY-POSITION

                *> Locate the start of the body \r\n\r\n 0D0A0D0A
                PERFORM VARYING BODY-POSITION FROM 1 BY 1 UNTIL BODY-POSITION > FUNCTION LENGTH(REQUEST-BUFFER)
                    IF REQUEST-BUFFER(BODY-POSITION:4) = X"0D0A0D0A" THEN
                        COMPUTE BODY-POSITION = BODY-POSITION + 4
                        EXIT PERFORM
                    END-IF
                END-PERFORM

                IF BODY-POSITION > 0 THEN
                    MOVE REQUEST-BUFFER(BODY-POSITION:) TO REQUEST-BODY
                    DISPLAY "DEBUG: Extracted Request Body: " REQUEST-BODY
                ELSE
                    DISPLAY "DEBUG: No body found in POST/PUT request."
                END-IF
            END-IF

            DISPLAY "DEBUG: Exiting PARSE-INCOMING-REQUEST"
            EXIT PARAGRAPH.

       IDENTIFY-REQUEST-METHOD.
            DISPLAY "Identifying request method..."
            DISPLAY "DEBUG: Full REQUEST-BUFFER:"
            DISPLAY REQUEST-BUFFER *> Show full HTTP request

            *> Extract the first line of the request
            MOVE REQUEST-BUFFER TO REQUEST-LINE
            DISPLAY "DEBUG: Extracted REQUEST-LINE:"
            DISPLAY REQUEST-LINE *> Show first line of request

            *> Extract the HTTP method (first word)
            UNSTRING REQUEST-LINE DELIMITED BY ALL " "
                INTO HTTP-METHOD DUMMY-VAR

            DISPLAY "DEBUG: Extracted HTTP Method: " HTTP-METHOD.
            EXIT PARAGRAPH.

       HANDLE-ROUTING.
            DISPLAY "Calling external router...".

            CALL "getpid" RETURNING PROCESS-ID

            MOVE "/tmp/output_" TO RESPONSE-FILE
            STRING RESPONSE-FILE PROCESS-ID ".txt" DELIMITED BY SIZE INTO RESPONSE-FILE

            *> Define system command with output redirection
            MOVE "routes.sh " TO SHELL-COMMAND.
            STRING SHELL-COMMAND HTTP-METHOD " " URL-PATH " " REQUEST-BODY " > " RESPONSE-FILE " 2>&1"
                   DELIMITED BY SIZE INTO SHELL-COMMAND

            DISPLAY "DEBUG: Executing: " SHELL-COMMAND

            CALL "system" USING SHELL-COMMAND.

            DISPLAY "DEBUG: Router executed, reading response file: " RESPONSE-FILE

            OPEN INPUT RESPONSE-FILE

            READ RESPONSE-FILE INTO RESPONSE-BUFFER
                 AT END DISPLAY "ERROR: Response file not found!"
                 NOT AT END COMPUTE RESPONSE-SIZE = FUNCTION LENGTH(RESPONSE-BUFFER)
            END-READ

            CLOSE RESPONSE-FILE

            MOVE "rm " TO DELETE-COMMAND.
            STRING DELETE-COMMAND RESPONSE-FILE DELIMITED BY SIZE INTO DELETE-COMMAND.
            CALL "system" USING DELETE-COMMAND.

            DISPLAY "DEBUG: Response file read successfully. Size: " RESPONSE-SIZE

            PERFORM SEND-RESPONSE

            EXIT PARAGRAPH.
