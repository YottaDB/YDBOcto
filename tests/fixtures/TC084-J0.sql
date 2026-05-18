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
-- JOIN coverage. skiptest_s1 has SKIP '99,100,101' (single-level numeric range
-- 0..$CHAR(0)) so its visible rows are {1, 2, 200}. skipref is an unfiltered
-- helper table whose row set is {1, 2, 3, 99, 200}. The expected JOIN behavior
-- below verifies that the SKIP-filter is honored on each side of every JOIN
-- shape: rows the planner has dropped because of SKIP must not surface as
-- inner-side matches, nor as non-NULL rows in OUTER JOIN output.
CREATE TABLE skipref (id INTEGER PRIMARY KEY, label VARCHAR(16)) GLOBAL "^skipref" READONLY;

-- Baseline rows of each table, shown so the JOIN reasoning below is verifiable
-- without having to mentally re-apply the SKIP filter.
-- skipref has no filter; rows {1, 2, 3, 99, 200}.
SELECT * FROM skipref ORDER BY id;
-- skiptest_s1 has SKIP '99,100,101' over numeric range 0..$CHAR(0); rows {1, 2, 200}.
SELECT * FROM skiptest_s1 ORDER BY id;

-- INNER JOIN: only rows present on both sides; expect {1, 2, 200}.
SELECT skipref.id, skipref.label, skiptest_s1.name
  FROM skipref INNER JOIN skiptest_s1 ON skipref.id = skiptest_s1.id
  ORDER BY skipref.id;

-- LEFT JOIN (skipref <- skiptest_s1): all rows of skipref; right side NULL where
-- skiptest_s1 drops/lacks the id. Expect 99 to have NULL name (filtered out).
SELECT skipref.id, skipref.label, skiptest_s1.name
  FROM skipref LEFT JOIN skiptest_s1 ON skipref.id = skiptest_s1.id
  ORDER BY skipref.id;

-- RIGHT JOIN: all rows of skiptest_s1; left side NULL where skipref lacks the id.
-- skiptest_s1 yields {1, 2, 200}; all are present in skipref.
SELECT skipref.id, skipref.label, skiptest_s1.name
  FROM skipref RIGHT JOIN skiptest_s1 ON skipref.id = skiptest_s1.id
  ORDER BY skiptest_s1.id;

-- FULL JOIN: union of both sides with NULLs on unmatched. Expect 99 with NULL
-- name, and any skiptest_s1-only ids with NULL label.
SELECT skipref.id, skipref.label, skiptest_s1.id, skiptest_s1.name
  FROM skipref FULL JOIN skiptest_s1 ON skipref.id = skiptest_s1.id
  ORDER BY skipref.id, skiptest_s1.id;
