-- ##############################################################
-- #								#
-- # Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
-- # All rights reserved.					#
-- #								#
-- #	This source code contains the intellectual property	#
-- #	of its copyright holder(s), and is made available	#
-- #	under a license.  If you do not know the terms of	#
-- #	the license, please stop and do not read further.	#
-- #								#
-- ##############################################################

-- TNJ05 : OCTO295 : names schema : NATURAL JOINs remove columns with duplicate names from table

-- Test that duplicate columns are not retained in a NATURAL JOIN if * is specified in SELECT column list
SELECT * FROM names NATURAL JOIN names AS n2;
SELECT * FROM names NATURAL JOIN (select id, firstname as first, lastname from names) namesB;
SELECT * FROM names NATURAL JOIN (select id, firstname as first, lastname from names) namesB NATURAL JOIN names AS n2;

-- Test that duplicate columns are retained in a NATURAL JOIN if explicitly specified in SELECT column list
SELECT n1.id,n2.id FROM names n1 NATURAL JOIN names n2;
SELECT n1.id,n2.id,n1.firstname,n2.firstname FROM names n1 NATURAL JOIN names n2;
SELECT n3.lastname,n2.lastname,n1.lastname FROM names n1 NATURAL JOIN names n2 NATURAL JOIN names n3;

-- Test order of columns that show up in SELECT column list (common columns first, non-common columns next)
SELECT * FROM (SELECT id FROM names) n1 NATURAL JOIN (SELECT id FROM names) n2;
SELECT * FROM (SELECT firstname, id FROM names) n1 NATURAL JOIN (SELECT * FROM names) n2;
SELECT * FROM (SELECT * FROM names) n1 NATURAL JOIN (SELECT lastname, firstname FROM names) n2;
SELECT * FROM (SELECT * FROM names) n1 NATURAL JOIN (SELECT lastname, firstname FROM names) n2 NATURAL JOIN (SELECT id, lastname FROM names) n3;

