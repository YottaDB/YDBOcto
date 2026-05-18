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
-- SKIP and SKIPCONDITION combined on the same key column, exercising the
-- '$SELECT(...,1:0)!(<cond>)' OR-combinator we emit in both the key-column
-- path and the AIM-xref path of the YDBOcto#617 optimization. SKIP filters
-- the discrete value 50; SKIPCONDITION filters subscripts > 90 (i.e. 99 and
-- 100). Together they remove {50, 99, 100} from ^skipmax = {1, 2, 50, 99, 100},
-- leaving {1:apple, 2:banana}.
--
-- max(id)/min(id) exercise the key-column path; max(name)/min(name) exercise
-- the AIM-xref path. Both must apply the same combined filter.
CREATE TABLE skiptest_aim2 (
  id   INTEGER PRIMARY KEY SKIP '50' SKIPCONDITION "keys(""id"")>90",
  name VARCHAR(64)
) GLOBAL "^skipmax";
SELECT * FROM skiptest_aim2 ORDER BY id;
SELECT max(id) FROM skiptest_aim2;
SELECT min(id) FROM skiptest_aim2;
SELECT max(name) FROM skiptest_aim2;
SELECT min(name) FROM skiptest_aim2;
