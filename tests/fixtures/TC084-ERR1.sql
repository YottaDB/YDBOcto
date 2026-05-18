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
-- Expect a syntax error: the grammar's ddl_str_literal_value non-terminal rejects
-- an empty string literal, so SKIP '' is caught at parse time before validation runs.
CREATE TABLE skiptest_err1 (
  id INTEGER PRIMARY KEY SKIP '',
  name VARCHAR(64)
) GLOBAL "^skiptest";
