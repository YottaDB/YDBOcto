#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TUT003 : OCTO579 : Test of simple UPDATE queries in composite database (multiple primary key columns)

-- Test UPDATE where 1 non primary key column is modified.

UPDATE composite SET name = 'Name11' where name = 'Name9';
SELECT * FROM composite;
UPDATE composite SET name = name || 'new1' where id1 > 1;
SELECT * FROM composite;
UPDATE composite SET name = name || 'new2' where id7 = 8;
SELECT * FROM composite;
UPDATE composite SET name = name || 'new3';
SELECT * FROM composite;

-- Test OR operator in WHERE clause works (tests DNF plan expansion logic)
-- Note: A similar query exists in "tests/fixtures/TUT010.sql". But it is here mostly to verify
-- that DNF plan expansion did happen whereas it is there in the TUT010.sql to verify correctness
-- of Octo output against Postgres.
UPDATE composite SET name = name || '?' where name = 'Name4' OR id4 = 5;
SELECT * FROM composite;

