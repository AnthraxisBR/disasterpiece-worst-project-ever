#!/bin/bash

# Router script that handles HTTP requests
# Usage: ./router.sh <method> <url> <body>

METHOD="$1"
URL="$2"
BODY="$3"

echo "HTTP/1.1 200 OK"
echo "Content-Type: text/plain"
echo "Connection: close"
echo ""
echo "Method: $METHOD"
echo "URL: $URL"
echo "Body: $BODY"
echo ""
echo "This is the response from the router script." 