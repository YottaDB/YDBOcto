#################################################################
#								#
# Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
CREATE TABLE skiptest_s6 (
  id INTEGER PRIMARY KEY START 0 ENDPOINT '$CHAR(0)' SKIP '99' SKIPCONDITION "keys(""id"")=100",
  name VARCHAR(64)
) GLOBAL "^skiptest";
SELECT * FROM skiptest_s6 ORDER BY id;
