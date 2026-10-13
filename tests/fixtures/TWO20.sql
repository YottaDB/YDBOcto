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

-- TWO20 : OCTO1158 : LIKE 'prefix%' walks the AIM cross reference over the prefix only
-- Each pattern is run twice. The first query (v LIKE ...) can use the cross reference of column v.
-- The second query (COALESCE(v, NULL) LIKE ...) is not on a column, so it never does: it checks every row.
-- Both must return the same rows.

CREATE TABLE TWO20 (id INTEGER PRIMARY KEY, v VARCHAR) GLOBAL "^TWO20";
INSERT INTO TWO20 VALUES (1,'PAT'),(2,'PATIENT'),(3,'PATENT'),(4,'PAU'),(5,'PA'),(6,'12'),(7,'120'),(8,'12a'),(9,'2');
INSERT INTO TWO20 VALUES (10,'PAT%X'),(11,'PAT_Y'),(12,NULL),(13,''),(14,'añb'),(15,'añ'),(16,'ao'),(17,'pat');

-- The table data
SELECT * FROM TWO20 ORDER BY id;

-- Prefix followed by only %
SELECT id FROM TWO20 WHERE v LIKE 'PAT%' ORDER BY id;
SELECT id FROM TWO20 WHERE COALESCE(v, NULL) LIKE 'PAT%' ORDER BY id;

-- Prefix followed by more than one %
SELECT id FROM TWO20 WHERE v LIKE 'PAT%%' ORDER BY id;
SELECT id FROM TWO20 WHERE COALESCE(v, NULL) LIKE 'PAT%%' ORDER BY id;

-- Prefix is the whole value of one row
SELECT id FROM TWO20 WHERE v LIKE 'PATIENT%' ORDER BY id;
SELECT id FROM TWO20 WHERE COALESCE(v, NULL) LIKE 'PATIENT%' ORDER BY id;

-- Prefix matches no row
SELECT id FROM TWO20 WHERE v LIKE 'ZZZ%' ORDER BY id;
SELECT id FROM TWO20 WHERE COALESCE(v, NULL) LIKE 'ZZZ%' ORDER BY id;

-- Prefix followed by more (the LIKE is still checked on each row)
SELECT id FROM TWO20 WHERE v LIKE 'PAT%ENT' ORDER BY id;
SELECT id FROM TWO20 WHERE COALESCE(v, NULL) LIKE 'PAT%ENT' ORDER BY id;

-- Prefix followed by _
SELECT id FROM TWO20 WHERE v LIKE 'PAT_ENT' ORDER BY id;
SELECT id FROM TWO20 WHERE COALESCE(v, NULL) LIKE 'PAT_ENT' ORDER BY id;

-- Escaped % in the prefix
SELECT id FROM TWO20 WHERE v LIKE 'PAT\%%' ORDER BY id;
SELECT id FROM TWO20 WHERE COALESCE(v, NULL) LIKE 'PAT\%%' ORDER BY id;

-- Escaped _ in the prefix
SELECT id FROM TWO20 WHERE v LIKE 'PAT\_%' ORDER BY id;
SELECT id FROM TWO20 WHERE COALESCE(v, NULL) LIKE 'PAT\_%' ORDER BY id;

-- Numeric-looking prefix: '12', '120' and '12a' match, '2' does not
SELECT id FROM TWO20 WHERE v LIKE '12%' ORDER BY id;
SELECT id FROM TWO20 WHERE COALESCE(v, NULL) LIKE '12%' ORDER BY id;

-- Multi-byte prefix
SELECT id FROM TWO20 WHERE v LIKE 'añ%' ORDER BY id;
SELECT id FROM TWO20 WHERE COALESCE(v, NULL) LIKE 'añ%' ORDER BY id;

-- Prefix that a multi-byte value starts with
SELECT id FROM TWO20 WHERE v LIKE 'a%' ORDER BY id;
SELECT id FROM TWO20 WHERE COALESCE(v, NULL) LIKE 'a%' ORDER BY id;

-- No prefix: starts with %
SELECT id FROM TWO20 WHERE v LIKE '%AT' ORDER BY id;
SELECT id FROM TWO20 WHERE COALESCE(v, NULL) LIKE '%AT' ORDER BY id;

-- No prefix: starts with _
SELECT id FROM TWO20 WHERE v LIKE '_AT%' ORDER BY id;
SELECT id FROM TWO20 WHERE COALESCE(v, NULL) LIKE '_AT%' ORDER BY id;

-- ~~ is the same as LIKE
SELECT id FROM TWO20 WHERE v ~~ 'PAT%' ORDER BY id;
SELECT id FROM TWO20 WHERE COALESCE(v, NULL) ~~ 'PAT%' ORDER BY id;

-- NOT LIKE (the rows without the prefix, which a walk over the prefix cannot find, so no cross reference)
SELECT id FROM TWO20 WHERE v NOT LIKE 'PAT%' ORDER BY id;
SELECT id FROM TWO20 WHERE COALESCE(v, NULL) NOT LIKE 'PAT%' ORDER BY id;

-- OR of two LIKE prefixes (each side of the OR walks over its own prefix)
SELECT id FROM TWO20 WHERE v LIKE 'PAT%' OR v LIKE '12%' ORDER BY id;
SELECT id FROM TWO20 WHERE COALESCE(v, NULL) LIKE 'PAT%' OR COALESCE(v, NULL) LIKE '12%' ORDER BY id;

-- A view stores its definition in binary form, so the LIKE inside it uses the cross reference too
CREATE VIEW TWO20view AS SELECT id, v FROM TWO20 WHERE v LIKE 'PAT%';
SELECT id FROM TWO20view ORDER BY id;
SELECT id FROM TWO20 WHERE COALESCE(v, NULL) LIKE 'PAT%' ORDER BY id;

-- ILIKE: the cross reference is in case-sensitive order, so the prefix ends at the first letter (as in PostgreSQL).
-- A prefix without letters followed by only % matches exactly what LIKE does: '12%' uses the same plan as LIKE '12%'.
SELECT id FROM TWO20 WHERE v ILIKE '12%' ORDER BY id;
SELECT id FROM TWO20 WHERE COALESCE(v, NULL) ILIKE '12%' ORDER BY id;

-- ILIKE with a letter after the prefix: walks over '12', then checks the ILIKE on each row ('12a' matches 'A')
SELECT id FROM TWO20 WHERE v ILIKE '12A%' ORDER BY id;
SELECT id FROM TWO20 WHERE COALESCE(v, NULL) ILIKE '12A%' ORDER BY id;

-- ILIKE without wildcards, escapes or letters is an "=", like LIKE '12' (as PostgreSQL does)
SELECT id FROM TWO20 WHERE v ILIKE '12' ORDER BY id;
SELECT id FROM TWO20 WHERE COALESCE(v, NULL) ILIKE '12' ORDER BY id;

-- ILIKE that starts with a letter has no prefix, so it reads every row
SELECT id FROM TWO20 WHERE v ILIKE 'pat%' ORDER BY id;
SELECT id FROM TWO20 WHERE COALESCE(v, NULL) ILIKE 'pat%' ORDER BY id;

-- A pattern that is a cast (not a string literal) is not looked at for a prefix, so the LIKE reads every row
SELECT id FROM TWO20 WHERE v LIKE 'PAT%'::VARCHAR ORDER BY id;
SELECT id FROM TWO20 WHERE COALESCE(v, NULL) LIKE 'PAT%'::VARCHAR ORDER BY id;

-- A key column (k) is not walked over a prefix. Its rows are read in the order of the table's own global, ^TWO20key(k),
-- where M sorts canonical numbers first, in numeric order, and all other strings after them: 2, 12, 13, 120, '12a'.
-- The values that start with '12' (12, 120, '12a') are not one contiguous block there, as 13 is in between. A walk
-- that starts at '12' and stops at the first value that does not start with '12' would stop at 13, and miss 120 and
-- '12a'. So the plan reads every row and checks the LIKE on each, and returns all three.
CREATE TABLE TWO20key (k VARCHAR PRIMARY KEY) GLOBAL "^TWO20key";
INSERT INTO TWO20key VALUES ('12'),('120'),('12a'),('13'),('2');
SELECT k FROM TWO20key WHERE k LIKE '12%' ORDER BY k;
SELECT k FROM TWO20key WHERE COALESCE(k, NULL) LIKE '12%' ORDER BY k;

-- A column of a table stored in a local variable (pg_catalog.pg_settings): its cross reference would be in a local
-- variable too, where canonical numbers also sort first, so no prefix walk. The plan reads every row and checks the LIKE.
SELECT name FROM pg_settings WHERE vartype LIKE 'en%' ORDER BY name;
SELECT name FROM pg_settings WHERE COALESCE(vartype, NULL) LIKE 'en%' ORDER BY name;

-- A view with a LIKE in a scalar subquery of its select list. Selecting from the view checks that subquery again, with the
-- literals of the stored view, which are not plan parameters. A pattern without a prefix ('%AT') is still a LIKE then, and
-- is looked at for a prefix a second time. A pattern with a prefix ('PAT%') became the prefix operation at CREATE VIEW, so
-- it is not looked at again.
CREATE VIEW TWO20viewsub AS SELECT id, (SELECT t.v FROM TWO20 t WHERE t.v LIKE '%AT' ORDER BY t.v LIMIT 1) AS x FROM TWO20 WHERE id < 3;
SELECT id, x FROM TWO20viewsub ORDER BY id;
SELECT id, (SELECT t.v FROM TWO20 t WHERE COALESCE(t.v, NULL) LIKE '%AT' ORDER BY t.v LIMIT 1) AS x FROM TWO20 WHERE id < 3 ORDER BY id;
CREATE VIEW TWO20viewsub2 AS SELECT id, (SELECT t.v FROM TWO20 t WHERE t.v LIKE 'PAT%' ORDER BY t.v LIMIT 1) AS x FROM TWO20 WHERE id < 3;
SELECT id, x FROM TWO20viewsub2 ORDER BY id;
SELECT id, (SELECT t.v FROM TWO20 t WHERE COALESCE(t.v, NULL) LIKE 'PAT%' ORDER BY t.v LIMIT 1) AS x FROM TWO20 WHERE id < 3 ORDER BY id;
