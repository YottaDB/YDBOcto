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
-- Run in UTF-8 mode only (see TRTE12 in "regex_type_expressions.bats.in").
-- ILIKE 'prefix%' with non-ASCII characters. Octo, like PostgreSQL, compares the start of the value with the literal,
-- both in lower case.
-- TRTE12d: an accented word in either case, a German sharp s, whose upper case is two letters (SS), a long s (ſ) and a
-- dotless i (ı), whose upper case is S and I but whose lower case is not s and i, an I with a dot (İ) and a Kelvin sign
-- (K), whose lower case is i and k, and a NULL.
DROP TABLE IF EXISTS TRTE12d;
CREATE TABLE TRTE12d (id INTEGER PRIMARY KEY, v VARCHAR);
INSERT INTO TRTE12d VALUES (1,'ÉTÉ'),(2,'été'),(3,'STRASSE'),(4,'straße'),(5,'ſx'),(6,'ıx'),(7,'İx'),(8,'Kx'),(9,NULL);
SELECT * FROM TRTE12d;

-- Expected: 1 ÉTÉ, 2 été
SELECT id, v FROM TRTE12d WHERE v ILIKE 'été%' ORDER BY id;
-- Expected: 4 straße (not 3 STRASSE)
SELECT id, v FROM TRTE12d WHERE v ILIKE 'STRAßE%' ORDER BY id;
-- Expected: 3 STRASSE (not 4 straße)
SELECT id, v FROM TRTE12d WHERE v ILIKE 'strasse%' ORDER BY id;
-- Expected: 3 STRASSE, 4 straße (not 5 ſx)
SELECT id, v FROM TRTE12d WHERE v ILIKE 's%' ORDER BY id;
-- Expected: 7 İx (not 6 ıx)
SELECT id, v FROM TRTE12d WHERE v ILIKE 'i%' ORDER BY id;
-- Expected: 8 Kx
SELECT id, v FROM TRTE12d WHERE v ILIKE 'k%' ORDER BY id;
-- Expected: every row but 1, 2 and 9 (NULL)
SELECT id, v FROM TRTE12d WHERE v NOT ILIKE 'ÉTÉ%' ORDER BY id;
