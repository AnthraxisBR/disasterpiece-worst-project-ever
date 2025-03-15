source ./modules/webserver/template.sh

template_generated=$(generate_template "index" "name=Test,")

request=$(socat - TCP-LISTEN:8081,crlf,reuseaddr)
while true; do
#  content_length=$(echo "$template_generated" | wc -l)
  http_response="HTTP/1.1 200 OK\r\n"
  echo "$request"

  if [[ "$request" =~ GET\ (.*)\ HTTP/1.1 ]]; then
    url_path=$(echo "${BASH_REMATCH[1]}" | tr -d '/')
    echo "url_path: $url_path"
    template_generated=$(generate_template "index" "name=$url_path,")
#    cat "$template_generated"
  fi
  request="$http_response\r\n"$(cat "$template_generated")
  echo $request | socat - TCP:localhost:8080,crlf,reuseaddr
done