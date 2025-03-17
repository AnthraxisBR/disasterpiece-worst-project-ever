
       01 SOCKET-FD            PIC 9(1) COMP-5.
       01 CLIENT-FD            PIC 9(1) COMP-5.
       01 SERVER-FD            PIC 9(1) COMP-5.

       01 CLIENT-ADDRESS.
          05 FAMILY            PIC 9(4) COMP-5 VALUE 2.           *> sin_family (AF_INET)
          05 PORT              PIC 9(4) COMP-5 VALUE 8080.        *> sin_port (network byte order)
          05 IP-ADDRESS        PIC X(4) VALUE X"00000000".        *> sin_addr (4-byte IP address)
          05 RESERVED          PIC X(8) VALUE LOW-VALUES.         *> sin_zero (padding)

       01 REQUEST-BUFFER       PIC X(1024).
       01 REQUEST-SIZE         PIC 9(9) COMP-5.
       01 RESPONSE             PIC X(128) VALUE
          "HTTP/1.1 200 OK" & X"0D0A" &
          "Content-Length: 0000" & X"0D0A" &
          "Connection: close" & X"0D0A" &
          X"0D0A" & "OK".
       01 RESPONSE-BODY        PIC X(128) VALUE X"0D0A" & "OK".

       01 RESPONSE-SIZE        PIC 9(9) COMP-5 VALUE 52.
       01 RESPONSE-SIZE-NUM    PIC 9(2).
       01 RESPONSE-SIZE-STRING PIC X(2).
       01 RESPONSE-BODY-SIZE   PIC 9(9) COMP-5 VALUE 4.
       01 RESPONSE-SPACE-COUNT PIC 9(9) COMP-5.
       01 RETURN-CODE-LOCAL    PIC S9(9) COMP-5.
       01 I                    PIC 9(3) COMP-5 VALUE 128.

       01 GET-POSITION         PIC 9(3) COMP-5.
       01 TEMP-REQUEST-LINE    PIC X(1024).
       01 HTTP-METHOD          PIC X(10).
       01 REQUEST-METHOD       PIC X(10).
       01 URL-PATH             PIC X(1024).
       01 DUMMY-VAR            PIC X(1024).
       01 CONTENT-TYPE         PIC X(1024).
       01 REQUEST-LINE         PIC X(1024).
       01 REQUEST-BODY         PIC X(1024).
       01 BODY-POSITION        PIC 9(3) COMP-5.

       01 SHELL-COMMAND        PIC X(1024).
       01 RESPONSE-FILE        PIC X(1024).

       01 PROCESS-ID           PIC 9(9) COMP-5.
