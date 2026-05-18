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
-- WHERE on a SKIP-filtered table. The planner may push the WHERE down into
-- the FOR-loop's QUIT condition; we want to confirm SKIP's QUIT line and the
-- WHERE filter coexist without interfering. Targets skiptest_s1 (SKIP 99,100,101).
SELECT * FROM skiptest_s1 WHERE id = 2 ORDER BY id;
SELECT * FROM skiptest_s1 WHERE id = 99 ORDER BY id;
SELECT * FROM skiptest_s1 WHERE id > 50 ORDER BY id;
