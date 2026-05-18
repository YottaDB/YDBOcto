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
-- WHERE on a SKIPCONDITION-filtered table. The SKIPCONDITION drops the
-- 50<id<200 range; the WHERE narrows further. Targets skiptest_s5.
SELECT * FROM skiptest_s5 WHERE id = 200 ORDER BY id;
SELECT * FROM skiptest_s5 WHERE id < 100 ORDER BY id;
SELECT * FROM skiptest_s5 WHERE id BETWEEN 1 AND 100 ORDER BY id;
