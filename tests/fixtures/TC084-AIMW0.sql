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
-- AIM-as-row-scan: `SELECT * ... WHERE non_key_column = <value>` activates the
-- AIM cross-reference to find candidate rows by walking the column index
-- (rather than scanning the table FOR loop). When SKIP / SKIPCONDITION is set
-- on the key column, each candidate key the AIM walk surfaces must still be
-- filtered, or the query returns rows from SKIP'd subscripts via the AIM path.
--
-- This is a different code path from TC084-AIM0 / TC084-AIM1 (which exercise
-- the OCTO617 MAX/MIN one-shot AIM walk).  TC084-AIMW0 exercises AIM as a
-- row-scan index for a `SELECT *` query with a `WHERE name = ...` predicate.
--
-- ^skipmax rows are {1:apple, 2:banana, 50:cherry, 99:zebra, 100:yam} with
-- SKIP '50,99,100' filtering the upper three ids. So:
--   WHERE name='apple'  -> matches id=1   -> NOT SKIP'd -> 1 row.
--   WHERE name='cherry' -> matches id=50  -> SKIP'd     -> 0 rows.
--   WHERE name='zebra'  -> matches id=99  -> SKIP'd     -> 0 rows.
--   WHERE name='yam'    -> matches id=100 -> SKIP'd     -> 0 rows.
--   WHERE name='banana' -> matches id=2   -> NOT SKIP'd -> 1 row.
CREATE TABLE skiptest_aimw0 (
  id   INTEGER PRIMARY KEY SKIP '50,99,100',
  name VARCHAR(64)
) GLOBAL "^skipmax";
SELECT * FROM skiptest_aimw0 WHERE name = 'apple' ORDER BY id;
SELECT * FROM skiptest_aimw0 WHERE name = 'cherry' ORDER BY id;
SELECT * FROM skiptest_aimw0 WHERE name = 'zebra' ORDER BY id;
SELECT * FROM skiptest_aimw0 WHERE name = 'yam' ORDER BY id;
SELECT * FROM skiptest_aimw0 WHERE name = 'banana' ORDER BY id;
