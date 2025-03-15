#!/bin/bash

source ./config.sh

ENTRY_TABLE_FILE=$DB_DIRECTORY"tables/entry.json"
ENTRY_ID_FILE=$DB_DIRECTORY"id/entry_ids.json"
ENTRY_RELATION_FILE=$DB_DIRECTORY"relations/entry_to_account.json"


#ENTRY_MODEL='{
#    "id": "number",
#    "account_id": "number",
#    "amount": "number",
#    "date": "string",
#}

get_latest_entry_id() {
  current_entry_id=$(jq -r '.[-1]' "$ENTRY_ID_FILE")
  # if theres no entry yet, set the current_entry_id to 0
  if [ "$current_entry_id" == "null" ]; then
    current_entry_id=0
  fi
  echo "$current_entry_id"
}

list_entries() {
    jq -r '.[] | .' "$ENTRY_TABLE_FILE"
}

get_entry() {
    jq -r ".[] | select(.id == $1)" "$ENTRY_TABLE_FILE"
}

add_entry() {
    latest_entry_id=$(get_latest_entry_id)
    id=$(("$latest_entry_id" + 1))
    account_id=$1
    account_exists=$(account_id_exists "$account_id")
    if [ "$account_exists" == "false" ]; then
        echo "Account with ID $account_id does not exist"
        return 0
    fi
    amount=$2
    date=$3
    description=$4
    entry_object="{\"id\":$id, \"account_id\": $account_id, \"amount\": $amount, \"date\": \"$date\", \"description\": \"$description\"}"
    jq ". += [$entry_object]" "$ENTRY_TABLE_FILE" > tmp_entry.json
    jq ". += [$id]" "$ENTRY_ID_FILE" > tmp_entry_ids.json
    entry_to_account_relation="[$id, $account_id]"
    jq ". += [$entry_to_account_relation]" "$ENTRY_RELATION_FILE" > tmp_entry_to_account.json

    mv tmp_entry.json "$ENTRY_TABLE_FILE"
    mv tmp_entry_ids.json "$ENTRY_ID_FILE"
    mv tmp_entry_to_account.json "$ENTRY_RELATION_FILE"

    echo "$entry_object" | jq
}