       PROCESS-REQUEST.
           DISPLAY "Processing request..."
           PERFORM PARSE-INCOMING-REQUEST
           DISPLAY "Request processed."

           DISPLAY "Handling routing..."
           PERFORM HANDLE-ROUTING
           DISPLAY "Routing handled."

           EXIT PARAGRAPH.

       PARSE-INCOMING-REQUEST.
           DISPLAY "Parsing request..."

           *> Identify HTTP method and URL path
           PERFORM IDENTIFY-REQUEST-METHOD

           *> Extract URL Path (between METHOD and " HTTP/1.1")
           UNSTRING REQUEST-BUFFER DELIMITED BY ALL " "
               INTO HTTP-METHOD URL-PATH DUMMY-VAR

           *> Trim extracted values
           MOVE URL-PATH TO TRIM-VALUE
           PERFORM CLEANUP-VALUE
           MOVE TRIM-VALUE TO URL-PATH

           DISPLAY "DEBUG: Extracted HTTP Method: " HTTP-METHOD
           DISPLAY "DEBUG: Extracted URL Path: " URL-PATH

           *> Handle request body for POST/PUT
           IF HTTP-METHOD = "POST" OR HTTP-METHOD = "PUT" THEN
               PERFORM EXTRACT-REQUEST-BODY
           END-IF

           DISPLAY "DEBUG: Exiting PARSE-INCOMING-REQUEST"
           EXIT PARAGRAPH.

       IDENTIFY-REQUEST-METHOD.
           DISPLAY "Identifying request method..."
           MOVE REQUEST-BUFFER TO REQUEST-LINE

           *> Extract the HTTP method (first word)
           UNSTRING REQUEST-LINE DELIMITED BY ALL " "
               INTO HTTP-METHOD DUMMY-VAR

           *> Trim HTTP method
           MOVE HTTP-METHOD TO TRIM-VALUE
           PERFORM CLEANUP-VALUE
           MOVE TRIM-VALUE TO HTTP-METHOD

           DISPLAY "DEBUG: Extracted HTTP Method: " HTTP-METHOD
           EXIT PARAGRAPH.

       EXTRACT-REQUEST-BODY.
           MOVE 0 TO BODY-POSITION

           *> Locate the start of the body (\r\n\r\n sequence)
           PERFORM VARYING BODY-POSITION FROM 1 BY 1
               UNTIL BODY-POSITION > FUNCTION LENGTH(REQUEST-BUFFER)
               IF REQUEST-BUFFER(BODY-POSITION:4) = X"0D0A0D0A" THEN
                   COMPUTE BODY-POSITION = BODY-POSITION + 4
                   EXIT PERFORM
               END-IF
           END-PERFORM

           IF BODY-POSITION > 0 THEN
               MOVE REQUEST-BUFFER(BODY-POSITION:) TO REQUEST-BODY

               *> Trim extracted body
               MOVE REQUEST-BODY TO TRIM-VALUE
               PERFORM CLEANUP-VALUE
               MOVE TRIM-VALUE TO REQUEST-BODY

               DISPLAY "DEBUG: Extracted Request Body: " REQUEST-BODY
           ELSE
               DISPLAY "DEBUG: No body found in POST/PUT request."
           END-IF

           EXIT PARAGRAPH.

       CLEANUP-VALUE.
           *> This routine removes leading/trailing spaces and line breaks
           MOVE FUNCTION TRIM(TRIM-VALUE) TO TRIM-VALUE
           EXIT PARAGRAPH.


       HANDLE-ROUTING.
            DISPLAY "Calling external router..."

            DISPLAY "DEBUG: HTTP-METHOD: " HTTP-METHOD
            DISPLAY "DEBUG: URL-PATH: " URL-PATH
            DISPLAY "DEBUG: REQUEST-BODY: " REQUEST-BODY
            CALL STATIC "router_wrapper"
                USING BY REFERENCE HTTP-METHOD
                      BY REFERENCE URL-PATH
                      BY REFERENCE REQUEST-BODY
                      BY REFERENCE RESPONSE-BUFFER
                      BY VALUE LENGTH OF RESPONSE-BUFFER
                RETURNING RESPONSE-SIZE


            DISPLAY "DEBUG: Router response received."
            DISPLAY "Response: " RESPONSE-BUFFER

            PERFORM SEND-RESPONSE

            EXIT PARAGRAPH.

       TRIM-SPACES.
           *> Find the first non-space/non-linebreak character (leading trim)
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > FUNCTION LENGTH(TRIM-VALUE)
               IF TRIM-VALUE(I:1) NOT = " " AND
                  TRIM-VALUE(I:1) NOT = X"0A" AND
                  TRIM-VALUE(I:1) NOT = X"0D"
                   EXIT PERFORM
               END-IF
           END-PERFORM
           MOVE TRIM-VALUE(I:) TO TRIM-VALUE

           *> Find the last non-space/non-linebreak character (trailing trim)
           MOVE FUNCTION LENGTH(TRIM-VALUE) TO TRIM-LENGTH
           PERFORM VARYING J FROM TRIM-LENGTH BY -1 UNTIL J = 1
               IF TRIM-VALUE(J:1) NOT = " " AND
                  TRIM-VALUE(J:1) NOT = X"0A" AND
                  TRIM-VALUE(J:1) NOT = X"0D"
                   EXIT PERFORM
               END-IF
           END-PERFORM
           MOVE TRIM-VALUE(1:J) TO TRIM-VALUE

           EXIT PARAGRAPH.
