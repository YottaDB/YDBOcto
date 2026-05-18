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
-- WHERE on a multi-level SKIP+SKIPCONDITION table. Both keys are filtered by
-- the table definition and by the WHERE clause simultaneously. Targets
-- skiptest_m3 (outer SKIP "east", inner SKIPCONDITION id=2).
SELECT * FROM skiptest_m3 WHERE region = 'west' ORDER BY region, id;
SELECT * FROM skiptest_m3 WHERE id > 1 ORDER BY region, id;
SELECT * FROM skiptest_m3 WHERE region = 'west' AND id = 3 ORDER BY region, id;
SELECT * FROM skiptest_m3 WHERE region = 'east' ORDER BY region, id;
