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
-- TRTE12: values that start with pat in any case, one shorter than pat, one with pat later on, PA%T (for the escape),
-- and a NULL.
-- TRTE12b: for the subquery, the join and the UPDATE.
DROP TABLE IF EXISTS TRTE12;
DROP TABLE IF EXISTS TRTE12b;
DROP TABLE IF EXISTS TRTE12c;
DROP TABLE IF EXISTS TRTE12e;
CREATE TABLE TRTE12 (id INTEGER PRIMARY KEY, v VARCHAR);
INSERT INTO TRTE12 VALUES (1,'PAT'),(2,'Patient'),(3,'pA'),(4,'xpat'),(5,'pa%t'),(6,NULL);
CREATE TABLE TRTE12b (id INTEGER PRIMARY KEY, w VARCHAR);
INSERT INTO TRTE12b VALUES (1,'pATX'),(2,'q'),(3,'R'),(4,NULL),(5,'Q');
SELECT * FROM TRTE12;
SELECT * FROM TRTE12b;

-- ############################################################################################################
-- v ILIKE 'pat%': the left operand is a column, so the plan works out the prefix pat once, before its loop, and compares
-- the start of each value, in lower case, with it. No regex.
-- Expected: t t f f f, and NULL (an empty value) for row 6
SELECT id, v, v ILIKE 'pat%' AS ilike FROM TRTE12 ORDER BY id;

-- ############################################################################################################
-- COALESCE(v, NULL) ILIKE 'pat%': the left operand is not a column, so the plan matches the regex on each row.
-- Expected: the same as above
SELECT id, v, COALESCE(v, NULL) ILIKE 'pat%' AS ilike FROM TRTE12 ORDER BY id;

-- ############################################################################################################
-- ILIKE and NOT ILIKE in a WHERE clause. No cross reference is walked, as 'pat%' has no prefix that is the same in either
-- case. The NULL row is returned by neither.
-- Expected: 1 PAT, 2 Patient; then 3 pA, 4 xpat, 5 pa%t
SELECT id, v FROM TRTE12 WHERE v ILIKE 'pat%' ORDER BY id;
SELECT id, v FROM TRTE12 WHERE v NOT ILIKE 'pat%' ORDER BY id;

-- ############################################################################################################
-- An escaped % is part of the literal: 'PA\%%' is the literal pa%, then %.
-- Expected: 5 pa%t
SELECT id, v FROM TRTE12 WHERE v ILIKE 'PA\%%' ORDER BY id;

-- ############################################################################################################
-- Two ILIKEs in one query: the plan works out each prefix once, before its loop, and checks each row against both.
-- NOT ILIKE 'pat%' keeps 3 pA, 4 xpat and 5 pa%t; NOT ILIKE 'x%' keeps 1 PAT, 2 Patient, 3 pA and 5 pa%t.
-- Expected: 3 pA, 5 pa%t
SELECT id, v FROM TRTE12 WHERE v NOT ILIKE 'pat%' AND v NOT ILIKE 'x%' ORDER BY id;

-- ############################################################################################################
-- A LIKE 'prefix%' and an ILIKE 'pat%' in one query: both are checked by comparing the start of the value, LIKE as is
-- and ILIKE in lower case.
-- Expected: 2 Patient
SELECT id, v FROM TRTE12 WHERE v ILIKE 'pat%' AND v NOT LIKE 'PAT%' ORDER BY id;

-- ############################################################################################################
-- NOT ILIKE OR another condition: the plan of each side of the OR checks its own condition.
-- Expected: 1 PAT (id = 1), 3 pA, 4 xpat, 5 pa%t
SELECT id, v FROM TRTE12 WHERE v NOT ILIKE 'pat%' OR id = 1 ORDER BY id;

-- ############################################################################################################
-- An ILIKE in the query and another in a correlated subquery: each plan works out its own prefix.
-- The query keeps 1 PAT and 2 Patient. Of their TRTE12b rows, only 2 q is ILIKE 'Q%'.
-- Expected: 2
SELECT id FROM TRTE12 WHERE v ILIKE 'pat%' AND EXISTS (SELECT 1 FROM TRTE12b WHERE TRTE12b.id = TRTE12.id AND w ILIKE 'Q%') ORDER BY id;

-- ############################################################################################################
-- NOT ILIKE in a join condition
-- Expected: 1 1 (w = pATX), 3 3 (w = R)
SELECT TRTE12.id, TRTE12b.id AS b_id FROM TRTE12 INNER JOIN TRTE12b ON TRTE12.id = TRTE12b.id AND w NOT ILIKE 'Q%' ORDER BY TRTE12.id;

-- ############################################################################################################
-- ILIKE in a CASE
-- Expected: y y n n n n (a NULL v is not ILIKE, so the ELSE)
SELECT id, CASE WHEN v ILIKE 'pat%' THEN 'y' ELSE 'n' END AS starts_pat FROM TRTE12 ORDER BY id;

-- ############################################################################################################
-- An ILIKE in an ORDER BY: the plan checks it in a one-line expression, not in the dotted lines above.
-- Expected: f first (3 pA, 4 xpat, 5 pa%t), then t (1 PAT, 2 Patient), then NULL (6)
SELECT id, v FROM TRTE12 ORDER BY v ILIKE 'pat%', id;

-- ############################################################################################################
-- ILIKE in a GROUP BY
-- Expected: f 3, t 2, NULL 1
SELECT v ILIKE 'pat%' AS starts_pat, COUNT(*) AS cnt FROM TRTE12 GROUP BY v ILIKE 'pat%' ORDER BY 1;

-- ############################################################################################################
-- NOT ILIKE in a HAVING
-- Expected: pA 1, pa%t 1, xpat 1
SELECT v, COUNT(*) AS cnt FROM TRTE12 GROUP BY v HAVING v NOT ILIKE 'pat%' ORDER BY v;

-- ############################################################################################################
-- NOT ILIKE in the SELECT of an INSERT INTO ... SELECT. The SELECT is a plan of its own, which sets the prefix. TRTE12c
-- also has a CHECK constraint with an ILIKE, which is checked as before (it is not an ILIKE on a column of a query).
-- Expected: 3 rows inserted (3 pA, 4 xpat, 5 pa%t)
CREATE TABLE TRTE12c (id INTEGER PRIMARY KEY, w VARCHAR CHECK (w NOT ILIKE 'z%'));
INSERT INTO TRTE12c SELECT id, v FROM TRTE12 WHERE v NOT ILIKE 'pat%';
SELECT * FROM TRTE12c ORDER BY id;

-- ############################################################################################################
-- NOT ILIKE in an UPDATE: the NULL w (4) is not updated
-- Expected: 3 rows updated, w becomes q!, R! and Q!
UPDATE TRTE12b SET w = w || '!' WHERE w NOT ILIKE 'PAT%';
SELECT * FROM TRTE12b ORDER BY id;

-- ############################################################################################################
-- NOT ILIKE in a DELETE: the NULL v (6) is not deleted
-- Expected: 3 rows deleted (3 pA, 4 xpat, 5 pa%t); 1 PAT, 2 Patient and 6 are left
DELETE FROM TRTE12 WHERE v NOT ILIKE 'pat%';
SELECT * FROM TRTE12 ORDER BY id;

-- ############################################################################################################
-- ILIKE '12A%': the prefix ends at the first letter, so it is '12', and the literal is 12a in lower case.
-- TRTE12e: values that start with 12a in either case, one that starts with 12 but not 12a, 12 alone, one with 12a later
-- on, and a NULL.
CREATE TABLE TRTE12e (id INTEGER PRIMARY KEY, v VARCHAR);
INSERT INTO TRTE12e VALUES (1,'12a'),(2,'12AB'),(3,'12b'),(4,'12'),(5,'x12a'),(6,NULL);
SELECT * FROM TRTE12e;

-- In a WHERE clause, the plan walks the cross reference of v over the prefix 12, and checks each row found by comparing
-- the start of the value, in lower case, with 12a. No regex.
-- Expected: 1 12a, 2 12AB
SELECT id, v FROM TRTE12e WHERE v ILIKE '12A%' ORDER BY id;

-- The same walk in descending order of v (YDBOcto#1160)
-- Expected: 1 12a, 2 12AB
SELECT id, v FROM TRTE12e WHERE v ILIKE '12A%' ORDER BY v DESC;

-- NOT ILIKE walks no cross reference: each row is checked the same way. The NULL row is not returned.
-- Expected: 3 12b, 4 12, 5 x12a
SELECT id, v FROM TRTE12e WHERE v NOT ILIKE '12A%' ORDER BY id;

-- In the select list
-- Expected: t t f f f, and NULL for row 6
SELECT id, v, v ILIKE '12A%' AS ilike FROM TRTE12e ORDER BY id;

-- ILIKE '12A%B' has more after its literal: the walk over 12 is kept, and each row found is matched with the regex.
-- Expected: 2 12AB
SELECT id, v FROM TRTE12e WHERE v ILIKE '12A%B' ORDER BY id;
