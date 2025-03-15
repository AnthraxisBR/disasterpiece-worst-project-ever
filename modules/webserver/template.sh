
generate_template() {
  local template_name=$1
  local placeholders="$2"
  local template_path="./templates/$template_name.html"

  cp "./templates/$template_name.html" "$template_path.tmp"

  IFS="," read -r -a key_value_pairs <<< "$placeholders"

  for pair in "${key_value_pairs[@]}"; do
     IFS="=" read -r key value <<< "$pair"
      sed -i "s/{{$key}}/$value/g" "$template_path.tmp"
  done


  if [ -f "$template_path" ]; then
    echo "$template_path.tmp"
  else
    echo "Template not found: $template_name"
  fi
}