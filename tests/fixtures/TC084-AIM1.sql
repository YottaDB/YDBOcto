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
-- AIM-xref path of YDBOcto#617 optimization combined with SKIPCONDITION (not
-- just SKIP). The key column's LVN is the walking variable of the AIM inner
-- FOR, so the keys("id") substitution inside the SKIPCONDITION expression
-- naturally reads the candidate AIM key being tested on each iteration --
-- no extra "SET <key-col-lvn>=<aim-iterator>" command is needed.
-- The user-supplied condition is "keys(""id"")>2" -- with
-- ^skipmax = {1:apple, 2:banana, 50:cherry, 99:zebra, 100:yam}, the rows whose
-- id > 2 are filtered, leaving {1:apple, 2:banana}. Expected: max(name) =
-- 'banana', min(name) = 'apple'.
CREATE TABLE skiptest_aim1 (
  id   INTEGER PRIMARY KEY SKIPCONDITION "keys(""id"")>2",
  name VARCHAR(64)
) GLOBAL "^skipmax";
SELECT * FROM skiptest_aim1 ORDER BY id;
SELECT max(name) FROM skiptest_aim1;
SELECT min(name) FROM skiptest_aim1;
