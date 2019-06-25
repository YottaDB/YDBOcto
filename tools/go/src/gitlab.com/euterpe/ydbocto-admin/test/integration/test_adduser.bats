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

@test "call ydbocto-admin add user with no user" {
  run ydbocto-admin add user
  [[ "$status" -eq 1 ]]
  [[ "$output" =~ "Usage" ]]
}

@test "call ydbocto-admin add user with one user" {
  echo tester | ydbocto-admin add user jon > results.log
  log=$(cat results.log)
  [[ "$log" =~ "Successfully added user: \"jon\"" ]]
}

@test "call ydbocto-admin add user with three users" {
  echo tester | ydbocto-admin add user jon > results.log
  log=$(cat results.log)
  [[ "$log" =~ "Successfully added user: \"jon\"" ]]

  echo tester | ydbocto-admin add user bobby > results.log
  log=$(cat results.log)
  [[ "$log" =~ "Successfully added user: \"bobby\"" ]]

  echo tester | ydbocto-admin add user suzy > results.log
  log=$(cat results.log)
  [[ "$log" =~ "Successfully added user: \"suzy\"" ]]
}
