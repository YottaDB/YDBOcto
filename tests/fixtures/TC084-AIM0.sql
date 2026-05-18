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
-- AIM-xref path of YDBOcto#617 optimization: MAX/MIN of a non-key column
-- automatically activates the AIM cross-reference. The AIM xref still indexes
-- SKIP-filtered subscripts, so without the OCTO1109 codegen fix the
-- optimization could return a column value belonging to a SKIP'd row.
--
-- ^skipmax is seeded with five rows whose names sort {apple, banana, cherry,
-- yam, zebra} corresponding to ids {1, 2, 50, 100, 99}. SKIP '50,99,100' filters
-- out {cherry, yam, zebra}, leaving {1:apple, 2:banana}. So MAX(name) must be
-- 'banana' and MIN(name) must be 'apple' -- a buggy implementation that ignored
-- SKIP on the key column would return 'zebra' for MAX, so the two distinct
-- expected values exercise SKIP filtering in the AIM-xref path.
CREATE TABLE skiptest_aim0 (
  id   INTEGER PRIMARY KEY SKIP '50,99,100',
  name VARCHAR(64)
) GLOBAL "^skipmax";
SELECT * FROM skiptest_aim0 ORDER BY id;
SELECT max(name) FROM skiptest_aim0;
SELECT min(name) FROM skiptest_aim0;
