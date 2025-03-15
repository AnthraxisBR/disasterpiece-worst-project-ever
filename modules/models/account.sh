#!/bin/bash

source ./config.sh

ACCOUNT_TABLE_FILE=$DB_DIRECTORY"tables/account.json"
ACCOUNT_ID_FILE=$DB_DIRECTORY"id/account_ids.json"

#ACCOUNT_MODEL='{
#    "id": "number",
#    "name": "string",
#    "initial_balance": "number"
#}'


get_latest_account_id() {
  current_account_id=$(get_index_in_file "$(cat "$ACCOUNT_ID_FILE")" 1)
  # if theres no account yet, set the current_account_id to 0
  if [ "$current_account_id" == "null" ]; then
    current_account_id=0
  fi
  echo "$current_account_id"
}

# Check if an account with the given ID exists
account_id_exists() {
  id=$1
  id_exists=$(jq --argjson index "$id" 'index($index) != null' "$ACCOUNT_ID_FILE")
  echo "$id_exists"
}

list_accounts() {
  jq -r '.[] | .' "$ACCOUNT_TABLE_FILE"
}

get_account() {
  id=$1
  jq -r ".[] | select(.id == $1)" "$ACCOUNT_TABLE_FILE"
}

add_account() {
  latest_account_id=$(get_latest_account_id)
  id=$(("$latest_account_id" + 1))
  name=$1
  if [ -z "$2" ]; then
      initial_balance=0
  else
      initial_balance=$2
  fi
  account_object="{\"id\":$id, \"name\": \"$name\", \"initial_balance\": $initial_balance}"
  jq ". += [$account_object]" "$ACCOUNT_TABLE_FILE" > tmp_account.json
  jq ". += [$id]" "$ACCOUNT_ID_FILE" > tmp_account_ids.json
  mv tmp_account.json "$ACCOUNT_TABLE_FILE"
  mv tmp_account_ids.json "$ACCOUNT_ID_FILE"
  echo "$account_object" | jq
}

remove_account() {
  id=$1
  jq "del(.[] | select(.id == $id))" "$ACCOUNT_TABLE_FILE" > tmp_account.json
  jq "del(.[] | select(. == $id))" "$ACCOUNT_ID_FILE" > tmp_account_ids.json
  mv tmp_account.json "$ACCOUNT_TABLE_FILE"
  mv tmp_account_ids.json "$ACCOUNT_ID_FILE"
  echo "true"
}

update_account() {
  id=$1
  name=$2
  initial_balance=$3
  account_object="{\"id\":$id, \"name\": \"$name\", \"initial_balance\": $initial_balance}"
  jq "map(if .id == $id then . + $account_object else . end)" "$ACCOUNT_TABLE_FILE" > tmp_account.json
  mv tmp_account.json "$ACCOUNT_TABLE_FILE"
  echo "$account_object" | jq
}
