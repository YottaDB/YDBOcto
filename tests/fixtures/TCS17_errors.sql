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

-- TCS17 : A bare literal like 't' in a CASE argument takes the type another argument resolves the CASE to

-- This query issues a type mismatch error, like Postgres, as a sub-query's 't' is a VARCHAR
SELECT CASE WHEN true THEN (SELECT 't') ELSE (1=1) END;

-- This query issues the same error, but with the sub-query in a later THEN argument resolved by an earlier one
SELECT CASE WHEN false THEN true WHEN true THEN (SELECT 't') END;

-- This CHECK constraint used to reject row 1, as 't' stayed a VARCHAR in a BOOLEAN CASE. Rows 1 and 3 are accepted.
CREATE TABLE TCS17a (id INTEGER PRIMARY KEY, a VARCHAR, b BOOLEAN
	CONSTRAINT ck CHECK ((CASE WHEN a = 'Zero' THEN 't' ELSE b END) = true));
INSERT INTO TCS17a VALUES (1, 'Zero', false);
INSERT INTO TCS17a VALUES (2, 'x', false);
INSERT INTO TCS17a VALUES (3, 'x', true);
SELECT * FROM TCS17a;

-- This CHECK constraint resolves its BOOLEAN type from the first THEN argument. Only row 2 is accepted.
CREATE TABLE TCS17b (id INTEGER PRIMARY KEY, a VARCHAR, b BOOLEAN
	CONSTRAINT ck CHECK (CASE WHEN a = 'Zero' THEN b WHEN a = 'One' THEN 'no' ELSE 'y' END));
INSERT INTO TCS17b VALUES (1, 'One', true);
INSERT INTO TCS17b VALUES (2, 'Two', false);
INSERT INTO TCS17b VALUES (3, 'Zero', false);
SELECT * FROM TCS17b;
DROP TABLE TCS17b;

-- This CHECK constraint resolves its BOOLEAN type from a later THEN argument, fixing the earlier 'no'. Only row 2 is accepted.
CREATE TABLE TCS17d (id INTEGER PRIMARY KEY, a VARCHAR, b BOOLEAN
	CONSTRAINT ck CHECK (CASE WHEN a = 'One' THEN 'no' WHEN a = 'Zero' THEN b ELSE 'y' END));
INSERT INTO TCS17d VALUES (1, 'One', true);
INSERT INTO TCS17d VALUES (2, 'Two', false);
INSERT INTO TCS17d VALUES (3, 'Zero', false);
SELECT * FROM TCS17d;
DROP TABLE TCS17d;

-- This CHECK constraint has a VARCHAR CASE type, which already worked. Only row 2 is accepted.
CREATE TABLE TCS17c (id INTEGER PRIMARY KEY, a VARCHAR
	CONSTRAINT ck CHECK ((CASE WHEN a = 'Zero' THEN 't' ELSE a END) <> 't'));
INSERT INTO TCS17c VALUES (1, 'Zero');
INSERT INTO TCS17c VALUES (2, 'u');
SELECT * FROM TCS17c;
