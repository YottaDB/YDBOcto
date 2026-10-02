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

-- TCTC018 : OCTO950 : Constraint errors from multi-row UPDATE match Postgres irrespective of row and SET clause order

-- Rows are inserted in the order id=2, id=1. Octo processes the UPDATE in primary key order and so updates id=1 first,
-- which then collides with the UNIQUE value 'Cereal' of id=2. Postgres issues the same error only if the crosscheck
-- kept its table in primary key order after the INSERT.
DROP TABLE IF EXISTS tctc018a;
CREATE TABLE tctc018a (id INTEGER PRIMARY KEY, firstName VARCHAR(30) UNIQUE, lastName VARCHAR(30));
INSERT INTO tctc018a VALUES (2, 'Cereal', NULL), (1, 'Lord', 'Killer');
UPDATE tctc018a SET firstName = nullif('Cereal', firstName), lastName = lastName;
SELECT * FROM tctc018a ORDER BY id;

-- The first UPDATE changes the primary key of the (2, 5) row to -59, which moves that row to the end of the Postgres
-- table but to the start of the Octo table. Octo processes the second UPDATE in primary key order and so does not
-- see a collision on id2 = -59. Postgres does the same only if the crosscheck kept its table in primary key order
-- after the first UPDATE.
DROP TABLE IF EXISTS tctc018c;
CREATE TABLE tctc018c (id1 INTEGER, id2 INTEGER, PRIMARY KEY (id2));
INSERT INTO tctc018c VALUES (7, 1);
INSERT INTO tctc018c VALUES (2, 5);
UPDATE tctc018c SET id2 = id1 - 61 WHERE (id2 > 2);
UPDATE tctc018c SET id1 = id1 - 67, id2 = id1 - 66;
SELECT * FROM tctc018c ORDER BY id2;

-- Rows are inserted in the order id=2, id=1. Updating id=1 violates the CHECK constraint on b. Updating id=2 sets a to
-- 6, which violates the UNIQUE constraint on a if id=1 has not been updated yet. Octo processes id=1 first and so issues
-- the CHECK constraint error. Postgres issues the same error only if the crosscheck kept its table in primary key order.
DROP TABLE IF EXISTS tctc018d;
CREATE TABLE tctc018d (id INTEGER PRIMARY KEY, a INTEGER UNIQUE, b INTEGER CHECK (b < 10));
INSERT INTO tctc018d VALUES (2, 5, 1), (1, 6, 9);
UPDATE tctc018d SET b = b + 1, a = CASE WHEN id = 2 THEN 6 ELSE a END;
SELECT * FROM tctc018d ORDER BY id;

-- Both NOT NULL columns are set to NULL, with the SET clause listing them in the reverse of the table column order.
-- Postgres reports the column that comes first in the table (salary), and so must Octo.
DROP TABLE IF EXISTS tctc018b;
CREATE TABLE tctc018b (id INTEGER PRIMARY KEY, salary NUMERIC(20,2) NOT NULL, yearsTenured INTEGER NOT NULL);
INSERT INTO tctc018b VALUES (1, 25000.01, 5), (2, 0, 0);
UPDATE tctc018b SET yearsTenured = NULL, salary = NULL;
UPDATE tctc018b SET yearsTenured = (SELECT yearsTenured * NULL), salary = 67.48 - NULL;
UPDATE tctc018b SET yearsTenured = nullif(0, yearsTenured), salary = nullif(0, salary);
SELECT * FROM tctc018b ORDER BY id;
