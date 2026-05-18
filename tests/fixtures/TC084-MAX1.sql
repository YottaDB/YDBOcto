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
-- Same as TC084-MAX0 but uses SKIPCONDITION instead of SKIP. SKIPCONDITION skips
-- numeric subscripts > 2, so after filtering only rows {1, 2} remain. MAX(id)
-- must be 2 and MIN(id) must be 1 -- pre-SKIP MAX would be 100, so this is a
-- meaningful regression check.
CREATE TABLE skiptest_max1 (
  id   INTEGER PRIMARY KEY SKIPCONDITION "keys(""id"")>2",
  name VARCHAR(64)
) GLOBAL "^skipmax";
SELECT * FROM skiptest_max1 ORDER BY id;
SELECT max(id) FROM skiptest_max1;
SELECT min(id) FROM skiptest_max1;
SELECT max(id), min(id) FROM skiptest_max1;
