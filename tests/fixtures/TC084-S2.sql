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
-- SKIP with quoted alphabetic values: skip subscripts "A" and "B" (string literals).
CREATE TABLE skiptest_s2 (
  id VARCHAR(16) PRIMARY KEY SKIP '"A","B"',
  name VARCHAR(64)
) GLOBAL "^skiptest" READONLY;
SELECT * FROM skiptest_s2 ORDER BY id;
