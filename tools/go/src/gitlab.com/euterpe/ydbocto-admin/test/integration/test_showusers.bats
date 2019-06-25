#################################################################
#								#
# Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

load test_helpers

setup() {
  init_test
  createdb
}

@test "call ydbocto-admin show users with no users" {
  ydbocto-admin show users >> results.log
  log=$(cat results.log)
  [[ "$log" =~ "No YDBOcto users found." ]]
}

@test "call ydbocto-admin show users with one user" {
  echo -n tester | ydbocto-admin add user jon >> results.log
  log=$(cat results.log)
  [[ "$log" =~ "Successfully added user: \"jon\"" ]]
  ydbocto-admin show users >> results.log
  log=$(cat results.log)
  [[ "$log" =~ "Current YDBOcto users, by ID:" ]]
  [[ "$log" =~ "jon" ]]
}

@test "call ydbocto-admin show users with three users" {
  echo -n tester | ydbocto-admin add user jon >> results.log
  log=$(cat results.log)
  [[ "$log" =~ "Successfully added user: \"jon\"" ]]
  echo -n tester | ydbocto-admin add user acteon >> results.log
  log=$(cat results.log)
  [[ "$log" =~ "Successfully added user: \"acteon\"" ]]
  echo -n tester | ydbocto-admin add user joe >> results.log
  log=$(cat results.log)
  [[ "$log" =~ "Successfully added user: \"joe\"" ]]

  ydbocto-admin show users >> results.log
  log=$(cat results.log)
  [[ "$log" =~ "Current YDBOcto users, by ID:" ]]
  [[ "$log" =~ "jon" ]]
  [[ "$log" =~ "acteon" ]]
  [[ "$log" =~ "joe" ]]
}
