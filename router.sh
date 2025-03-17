#!/bin/bash

METHOD=$HTTP_METHOD
PATH=$HTTP_PATH
BODY=$HTTP_BODY

echo "Routing: $METHOD $PATH"
echo "Body: $BODY"

case "$METHOD $PATH" in
  "GET /")
    echo "HTTP/1.1 200 OK"
    echo "Content-Type: text/html"
    echo "Content-Length: 53"
    echo ""
    echo "<html><body><h1>Welcome Home!</h1></body></html>"
    ;;
  "POST /submit")
    echo "HTTP/1.1 200 OK"
    echo "Content-Type: text/html"
    echo "Content-Length: 55"
    echo ""
    echo "<html><body><h1>Form Submitted: $BODY</h1></body></html>"
    ;;
  *)
    echo "HTTP/1.1 404 Not Found"
    echo "Content-Type: text/html"
    echo "Content-Length: 44"
    echo ""
    echo "<html><body><h1>404 Not Found</h1></body></html>"
    ;;
esac
