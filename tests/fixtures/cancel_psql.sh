#!/usr/bin/expect
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

set timeout 2
set port [lindex $argv 0];
spawn /bin/bash
# Enable long lines to prevent wrapping
send -- "stty cols 4096\r"
expect "stty cols 4096\r"
# Start psql
send -- "psql -U ydb -h localhost -p $port\r"
expect -timeout 2 ".*Password for user ydb: "
send "ydbrocks\r"
expect -timeout 2 ".*ydb=# "
# Run a long time AND generate cross references
send "select firstname from names where firstname = \"abcd\";\r"
expect -timeout 10 "select firstname from names where firstname = \"abcd\";\r\n"
expect timeout
# Send interrupt to trigger cancel request
send \x03
expect -timeout 10 ".*Cancel request sent\r\n"
expect -timeout 40 "ERROR:  canceling statement due to user request\r\nydb=# "
send "\\q\r"
expect timeout
exit
