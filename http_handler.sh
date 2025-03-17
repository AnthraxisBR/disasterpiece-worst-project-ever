#!/bin/bash

# Function to generate HTML dynamically based on the URL
generate_html() {
    url_path="$1"
    output_file="output.html"
    echo "<html><body><h1>Hello, $url_path!</h1></body></html>" > "$output_file"
}

# Read the incoming request line by line
while read -r line; do
    # Process only the first line of the request (the GET request)
    if [[ "$line" =~ GET\ /(.*)\ HTTP ]]; then
        url_path="${BASH_REMATCH[1]}"
        if [[ -z "$url_path" ]]; then
            url_path="world"  # Default if no path is provided
        fi

        # Generate the HTML based on the URL path
        generate_html "$url_path"

        # Send the HTTP response and close the connection
        {
            echo -e "HTTP/1.1 200 OK\r"
            echo -e "Content-Type: text/html\r"
            echo -e "Connection: close\r"  # Close the connection after response
            echo -e "\r"  # Empty line separating headers from body
            cat "output.html"
        }
    fi
done
