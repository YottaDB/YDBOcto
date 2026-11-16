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

-- ############################################################################################################
-- The data
-- TRTE11: two values that start with PAT, one shorter than PAT, PAT in lower case, and a NULL.
-- TRTE11b: for the subquery, the join and the UPDATE.
DROP TABLE IF EXISTS TRTE11;
DROP TABLE IF EXISTS TRTE11b;
DROP TABLE IF EXISTS TRTE11c;
CREATE TABLE TRTE11 (id INTEGER PRIMARY KEY, v VARCHAR);
INSERT INTO TRTE11 VALUES (1,'PAT'),(2,'PATIENT'),(3,'PA'),(4,'pat'),(5,NULL);
CREATE TABLE TRTE11b (id INTEGER PRIMARY KEY, w VARCHAR);
INSERT INTO TRTE11b VALUES (1,'PATX'),(2,'Q'),(3,'R'),(4,NULL);
SELECT * FROM TRTE11;
SELECT * FROM TRTE11b;

-- ############################################################################################################
-- v NOT LIKE 'PAT%': the left operand is a column, so the plan works out the prefix PAT once, before its loop, and
-- compares the start of each value with it. No regex.
-- Expected: f f t t, and NULL (an empty value) for row 5
SELECT id, v, v NOT LIKE 'PAT%' AS not_like FROM TRTE11 ORDER BY id;

-- ############################################################################################################
-- COALESCE(v, NULL) NOT LIKE 'PAT%': the left operand is not a column, so the plan matches the regex on each row.
-- Expected: the same as above
SELECT id, v, COALESCE(v, NULL) NOT LIKE 'PAT%' AS not_like FROM TRTE11 ORDER BY id;

-- ############################################################################################################
-- NOT LIKE in a WHERE clause (the query of YDBOcto#1189). The NULL row is not returned.
-- Expected: 3 PA, 4 pat
SELECT id, v FROM TRTE11 WHERE v NOT LIKE 'PAT%' ORDER BY id;

-- ############################################################################################################
-- Two LIKEs in one query: the plan works out each prefix once, before its loop, and checks each row against both.
-- NOT LIKE 'PAT%' keeps 3 PA and 4 pat; NOT LIKE 'pa%' keeps 1 PAT, 2 PATIENT and 3 PA.
-- Expected: 3 PA
SELECT id, v FROM TRTE11 WHERE v NOT LIKE 'PAT%' AND v NOT LIKE 'pa%' ORDER BY id;

-- ############################################################################################################
-- NOT LIKE OR another condition: the plan of each side of the OR checks its own condition.
-- Expected: 1 PAT (id = 1), 3 PA, 4 pat
SELECT id, v FROM TRTE11 WHERE v NOT LIKE 'PAT%' OR id = 1 ORDER BY id;

-- ############################################################################################################
-- A NOT LIKE in the query and another in a correlated subquery: each plan works out its own prefix.
-- The query keeps 3 PA and 4 pat. Of their TRTE11b rows, only 3 R is NOT LIKE 'Q%' (4 is NULL).
-- Expected: 3
SELECT id FROM TRTE11 WHERE v NOT LIKE 'PAT%' AND EXISTS (SELECT 1 FROM TRTE11b WHERE TRTE11b.id = TRTE11.id AND w NOT LIKE 'Q%') ORDER BY id;

-- ############################################################################################################
-- NOT LIKE in a join condition
-- Expected: 1 1 (w = PATX), 3 3 (w = R)
SELECT TRTE11.id, TRTE11b.id AS b_id FROM TRTE11 INNER JOIN TRTE11b ON TRTE11.id = TRTE11b.id AND w NOT LIKE 'Q%' ORDER BY TRTE11.id;

-- ############################################################################################################
-- LIKE in a CASE
-- Expected: y y n n n (a NULL v is not LIKE, so the ELSE)
SELECT id, CASE WHEN v LIKE 'PAT%' THEN 'y' ELSE 'n' END AS starts_pat FROM TRTE11 ORDER BY id;

-- ############################################################################################################
-- A LIKE in an ORDER BY: the plan checks it in a one-line expression, not in the dotted lines above.
-- Expected: f first (1 PAT, 2 PATIENT), then t (3 PA, 4 pat), then NULL (5)
SELECT id, v FROM TRTE11 ORDER BY v NOT LIKE 'PAT%', id;

-- ############################################################################################################
-- LIKE in a GROUP BY
-- Expected: f 2 (PA, pat), t 2 (PAT, PATIENT), NULL 1
SELECT v LIKE 'PAT%' AS starts_pat, COUNT(*) AS cnt FROM TRTE11 GROUP BY v LIKE 'PAT%' ORDER BY 1;

-- ############################################################################################################
-- NOT LIKE in a HAVING
-- Expected: PA 1, pat 1
SELECT v, COUNT(*) AS cnt FROM TRTE11 GROUP BY v HAVING v NOT LIKE 'PAT%' ORDER BY v;

-- ############################################################################################################
-- NOT LIKE in the SELECT of an INSERT INTO ... SELECT. The SELECT is a plan of its own, which sets the prefix. TRTE11c also
-- has a CHECK constraint with a LIKE, which is checked as before (it is not a LIKE on a column of a query).
-- Expected: 2 rows inserted (3 PA, 4 pat)
CREATE TABLE TRTE11c (id INTEGER PRIMARY KEY, w VARCHAR CHECK (w NOT LIKE 'Z%'));
INSERT INTO TRTE11c SELECT id, v FROM TRTE11 WHERE v NOT LIKE 'PAT%';
SELECT * FROM TRTE11c ORDER BY id;

-- ############################################################################################################
-- NOT LIKE in an UPDATE: the NULL w (4) is not updated
-- Expected: 2 rows updated, w becomes Q! and R!
UPDATE TRTE11b SET w = w || '!' WHERE w NOT LIKE 'PAT%';
SELECT * FROM TRTE11b ORDER BY id;

-- ############################################################################################################
-- NOT LIKE in a DELETE: the NULL v (5) is not deleted
-- Expected: 2 rows deleted (3 PA, 4 pat); 1 PAT, 2 PATIENT and 5 are left
DELETE FROM TRTE11 WHERE v NOT LIKE 'PAT%';
SELECT * FROM TRTE11 ORDER BY id;
