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
-- Baseline: maps ^skiptest with no SKIP/SKIPCONDITION so we see the unfiltered data.
CREATE TABLE skiptest_s0 (
  id VARCHAR(16) PRIMARY KEY,
  name VARCHAR(64)
) GLOBAL "^skiptest" READONLY;
SELECT * FROM skiptest_s0 ORDER BY id;
