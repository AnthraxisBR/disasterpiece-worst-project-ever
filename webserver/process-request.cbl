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


            CALL STATIC "router_wrapper"
                USING BY REFERENCE HTTP-METHOD
                      BY REFERENCE URL-PATH
                      BY REFERENCE REQUEST-BODY
                      BY REFERENCE RESPONSE-BUFFER
                      BY VALUE LENGTH OF RESPONSE-BUFFER
                RETURNING RESPONSE-SIZE.


            DISPLAY "DEBUG: Router response received."
            DISPLAY "Response: " RESPONSE-BUFFER.

            PERFORM SEND-RESPONSE

            EXIT PARAGRAPH.
