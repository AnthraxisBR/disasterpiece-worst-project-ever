

source ./modules/models/account.sh
source ./modules/models/entry.sh

account_id=$1

#account=$(add_account "Test Account" 100)
#account_id=$(jq -r '.id' <<< "$account")
#
get_account "$account_id"
#
#updated_account=$(update_account "$account_id" "Updated Account" 200)
#updated_account_id=$(jq -r '.id' <<< "$updated_account")
#
#get_account "$updated_account_id"
#
#remove_account 4

add_entry "$account_id" -120 "2025-01-01"