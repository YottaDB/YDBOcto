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
-- AIM-as-row-scan + SKIPCONDITION: same code path as TC084-AIMW0 but with the
-- predicate form of the filter.  SKIPCONDITION on the key column drops every
-- key whose id > 2, so any AIM-surfaced candidate beyond id=2 must be filtered
-- inside the row-scan DO body.
--
-- ^skipmax rows are {1:apple, 2:banana, 50:cherry, 99:zebra, 100:yam} with
-- SKIPCONDITION "keys(""id"")>2". So:
--   WHERE name='apple'  -> id=1   -> kept -> 1 row.
--   WHERE name='banana' -> id=2   -> kept -> 1 row.
--   WHERE name='cherry' -> id=50  -> dropped -> 0 rows.
--   WHERE name='zebra'  -> id=99  -> dropped -> 0 rows.
--   WHERE name='yam'    -> id=100 -> dropped -> 0 rows.
CREATE TABLE skiptest_aimw1 (
  id   INTEGER PRIMARY KEY SKIPCONDITION "keys(""id"")>2",
  name VARCHAR(64)
) GLOBAL "^skipmax";
SELECT * FROM skiptest_aimw1 WHERE name = 'apple' ORDER BY id;
SELECT * FROM skiptest_aimw1 WHERE name = 'banana' ORDER BY id;
SELECT * FROM skiptest_aimw1 WHERE name = 'cherry' ORDER BY id;
SELECT * FROM skiptest_aimw1 WHERE name = 'zebra' ORDER BY id;
SELECT * FROM skiptest_aimw1 WHERE name = 'yam' ORDER BY id;
