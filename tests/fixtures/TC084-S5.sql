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
CREATE TABLE skiptest_s5 (
  id INTEGER PRIMARY KEY START 0 ENDPOINT '$CHAR(0)' SKIPCONDITION "(keys(""id"")>50)&(keys(""id"")<200)",
  name VARCHAR(64)
) GLOBAL "^skiptest";
SELECT * FROM skiptest_s5 ORDER BY id;
