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
-- Expect ERR_VALUES_NOT_ALLOWED_IN_START_END since values() is not allowed in SKIPCONDITION.
CREATE TABLE skiptest_err0 (
  id INTEGER PRIMARY KEY SKIPCONDITION "values(""name"")=""skip""",
  name VARCHAR(64)
) GLOBAL "^skiptest";
