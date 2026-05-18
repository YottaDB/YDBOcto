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
CREATE TABLE skiptest_s4 (
  id VARCHAR(16) PRIMARY KEY SKIPCONDITION "keys(""id"")'?1N.N",
  name VARCHAR(64)
) GLOBAL "^skiptest";
SELECT * FROM skiptest_s4 ORDER BY id;
