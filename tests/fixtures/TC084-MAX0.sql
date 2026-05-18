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
-- The YDBOcto#617 MAX/MIN optimization replaces the FOR-loop scan with a single
-- $ORDER call. Without the OCTO1109 fix to tmpl_tablejoin_octo617_optimize, that
-- single call lands on a SKIP-filtered subscript silently and returns it as the
-- MAX/MIN. The codegen now wraps that $ORDER in a re-ordering FOR that walks
-- past SKIP'd values, so MAX/MIN honor SKIP.
--
-- ^skipmax is seeded with 5 numeric subscripts {1,2,50,99,100}. SKIP '50,99,100'
-- removes the upper three rows, leaving {1, 2}. So MAX(id) must be 2 and MIN(id)
-- must be 1 -- two distinct values, so a buggy implementation that returns the
-- pre-SKIP MAX of 100 cannot pass the test by accident.
CREATE TABLE skiptest_max0 (
  id   INTEGER PRIMARY KEY SKIP '50,99,100',
  name VARCHAR(64)
) GLOBAL "^skipmax";
SELECT * FROM skiptest_max0 ORDER BY id;
SELECT max(id) FROM skiptest_max0;
SELECT min(id) FROM skiptest_max0;
SELECT max(id), min(id) FROM skiptest_max0;
