

# Function to get the value of a key in a JSON file
get_index_in_file() {
  input=$1
  index=$2
  value=$(echo "$input" | tr -d '[]' | tr -d ' \n' | tr ',' '\n' | awk -v idx="$index" 'NR==idx-1')
  echo "$value"
}
