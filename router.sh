#!/bin/bash

METHOD=$HTTP_METHOD
PATH=$HTTP_PATH
BODY=$HTTP_BODY

echo "Routing: $METHOD $PATH"
echo "Body: $BODY"
