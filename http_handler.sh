#!/bin/bash

generate_html() {
    url_path="$1"
    output_file="output.html"
    echo "<html><body><h1>Hello, $url_path!</h1></body></html>" > "$output_file"
}

while read -r line; do
    if [[ "$line" =~ GET\ /(.*)\ HTTP ]]; then
        url_path="${BASH_REMATCH[1]}"
        if [[ -z "$url_path" ]]; then
            url_path="world"
        fi

        generate_html "$url_path"

        {
            echo -e "HTTP/1.1 200 OK\r"
            echo -e "Content-Type: text/html\r"
            echo -e "Connection: close\r"
            echo -e "\r"
            cat "output.html"
        }
    fi
done
