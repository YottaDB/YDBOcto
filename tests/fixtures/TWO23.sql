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
-- Part A (crosschecked against PostgreSQL)
-- The composite table: 8 key columns id0 to id7 and name. A WHERE on one key column after the first walks the cross
-- reference of that column, instead of the FOR loops of every key column before it.
-- The rows, in key order:
--   Name1  (0,1,2,3,4,5,6,7)  Name2  (0,1,2,3,4,5,6,8)  Name3  (0,1,2,3,4,5,7,7)  Name4  (0,1,2,3,4,5,8,7)
--   Name5  (0,1,2,3,4,6,8,7)  Name6  (0,1,2,3,5,6,8,7)  Name7  (0,1,2,4,5,6,8,7)  Name8  (0,1,3,4,5,6,8,7)
--   Name9  (0,2,3,4,5,6,8,7)  Name10 (1,2,3,4,5,6,8,7)
SELECT * FROM composite;

-- ############################################################################################################
-- Part B (crosschecked against PostgreSQL)
-- A WHERE on one later key column: the plan walks its cross reference (ORDER BY name sorts in every query).
-- xref and fixed id4: Name10 Name6 Name7 Name8 Name9
SELECT * FROM composite WHERE id4 = 5 ORDER BY name;
-- xref and fixed id7: Name2
SELECT * FROM composite WHERE id7 = 8 ORDER BY name;
-- xref of id6, fixed to 6 then 7: Name1 Name2 Name3
SELECT * FROM composite WHERE id6 IN (6, 7) ORDER BY name;
-- xref of id5, walked from 5: Name10 Name5 Name6 Name7 Name8 Name9
SELECT * FROM composite WHERE id5 > 5 ORDER BY name;
-- xref of id3, walked up to 3: Name1 Name2 Name3 Name4 Name5 Name6
SELECT * FROM composite WHERE id3 <= 3 ORDER BY name;
-- xref of id2, walked from 3 to 4: Name10 Name8 Name9
SELECT * FROM composite WHERE id2 BETWEEN 3 AND 4 ORDER BY name;
-- xref and fixed id1: no row has id1 = 9
SELECT * FROM composite WHERE id1 = 9 ORDER BY name;
-- xref and fixed id6: Name4 to Name10, 7 rows
SELECT COUNT(*) FROM composite WHERE id6 = 8;

-- ############################################################################################################
-- Part C (crosschecked against PostgreSQL)
-- Two later key columns, in both orders: the earlier one (id4) gets the cross reference, and the later one (id6) is
-- fixed in place under it. Name10 Name6 Name7 Name8 Name9
SELECT * FROM composite WHERE id4 = 5 AND id6 = 8 ORDER BY name;
SELECT * FROM composite WHERE id6 = 8 AND id4 = 5 ORDER BY name;
-- An earlier key column constrained as well: no cross reference; the earlier key is fixed, so the loops before the
-- later key run once, and the later key is fixed in place too.
-- id1 and id7 fixed: Name1 Name3 Name4 Name5 Name6 Name7 Name8
SELECT * FROM composite WHERE id7 = 7 AND id1 = 1 ORDER BY name;
-- id0 and id5 fixed: Name5 Name6 Name7 Name8 Name9
SELECT * FROM composite WHERE id0 = 0 AND id5 = 6 ORDER BY name;

-- ############################################################################################################
-- Part D (crosschecked against PostgreSQL)
-- JOINs whose ON fixes a later key column of the joined table (c2) to a value or to a column of the other table (c1).
-- c2.id6 = c1.id5: each of Name5 to Name10 (id5 = 6) with Name1 and Name2 (id6 = 6), 12 rows
SELECT c1.name, c2.name FROM composite c1 INNER JOIN composite c2 ON c2.id6 = c1.id5 ORDER BY 1, 2;
-- Name5 to Name10 (c1.id5 = 6), each with Name2 (c2.id7 = 8)
SELECT c1.name, c2.name FROM composite c1 LEFT JOIN composite c2 ON c2.id7 = 8 WHERE c1.id5 = 6 ORDER BY 1, 2;
-- No row has id7 = 9: Name5 to Name10, each with a NULL c2.name (shown empty)
SELECT c1.name, c2.name FROM composite c1 LEFT JOIN composite c2 ON c2.id7 = 9 WHERE c1.id5 = 6 ORDER BY 1, 2;
-- Name2 (c2.id7 = 8) with each of Name1 to Name5 (c1.id4 = 4); every other c2 row with a NULL c1.name, last
SELECT c1.name, c2.name FROM composite c1 RIGHT JOIN composite c2 ON c1.id4 = 4 AND c2.id7 = 8 ORDER BY 1, 2;
-- PostgreSQL runs a FULL JOIN only on a condition that compares the two tables. c2.id7 = c1.id6: Name3 (id6 = 7)
-- with the 9 rows whose id7 = 7, Name4 to Name10 (id6 = 8) with Name2, and Name1 and Name2 (id6 = 6) with NULL
SELECT c1.name, c2.name FROM composite c1 FULL JOIN composite c2 ON c2.id7 = c1.id6 ORDER BY 1, 2;

-- ############################################################################################################
-- Part E (crosschecked against PostgreSQL)
-- A later key column and a non-key column (name). A table gets one cross reference per query: the condition that
-- comes first gets it (of id4, or of name), and the other condition is checked on each row. Both: Name7
SELECT * FROM composite WHERE id4 = 5 AND name = 'Name7' ORDER BY name;
SELECT * FROM composite WHERE name = 'Name7' AND id4 = 5 ORDER BY name;
-- A row comparison (c1.* = c2.*) next to a condition on a later key column: (Name2, Name2)
SELECT c1.name, c2.name FROM composite c1 INNER JOIN composite c2 ON c1.* = c2.* WHERE c2.id7 = 8 ORDER BY 1, 2;

-- ############################################################################################################
-- Part F (crosschecked against PostgreSQL)
-- ORDER BY every key column, in both directions: the rows come in the order of the cross reference walk, without a
-- sort. A range on id4 walks the cross reference of id4 through its values; with id4 = 5, id0 to id3 advance from the
-- cross reference under that one value.
-- [no sort, ascending walk] Name6 Name7 Name8 Name9 Name10
SELECT * FROM composite WHERE id4 > 4 ORDER BY id4, id0, id1, id2, id3, id5, id6, id7;
-- [no sort, descending walk] Name10 Name9 Name8 Name7 Name6
SELECT * FROM composite WHERE id4 > 4 ORDER BY id4 DESC, id0 DESC, id1 DESC, id2 DESC, id3 DESC, id5 DESC, id6 DESC, id7 DESC;
-- [no sort] id4 is one value of the cross reference, and id0 to id3, then id5 to id7, come in key order under it:
-- Name6 Name7 Name8 Name9 Name10
SELECT * FROM composite WHERE id4 = 5 ORDER BY id0, id1, id2, id3, id5, id6, id7;
-- [no sort] the same, in descending key order: Name10 Name9 Name8 Name7 Name6
SELECT * FROM composite WHERE id4 = 5 ORDER BY id0 DESC, id1 DESC, id2 DESC, id3 DESC, id5 DESC, id6 DESC, id7 DESC;

-- ############################################################################################################
-- Part G (crosschecked against PostgreSQL)
-- UPDATE and DELETE with a WHERE on a later key column, on TWO23, a copy of composite. Each one is followed by the
-- rows of TWO23 after it.
-- The rows of the copy: the same as composite's
SELECT * FROM TWO23 ORDER BY name;
-- id6 = 8 is Name4 to Name10: each becomes Name<n>u
UPDATE TWO23 SET name = name || 'u' WHERE id6 = 8;
SELECT * FROM TWO23 ORDER BY name;
-- id7 = 7 is every row but Name2: id7 becomes 17, each row moved once
UPDATE TWO23 SET id7 = id7 + 10 WHERE id7 = 7;
SELECT * FROM TWO23 ORDER BY name;
-- id5 = 6 is Name5u to Name10u: Name1 Name2 Name3 Name4u remain
DELETE FROM TWO23 WHERE id5 = 6;
SELECT * FROM TWO23 ORDER BY name;
-- id7 > 15 is Name1, Name3 and Name4u (id7 = 17): Name2 remains
DELETE FROM TWO23 WHERE id7 > 15;
SELECT * FROM TWO23 ORDER BY name;
