#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/OR its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TSD04 : SELECT DISTINCT should issue error if ORDER BY expression does not appear in SELECT column list

-- All queries in this query file are valid queries that do not issue any error both in Octo and Postgres.

SELECT DISTINCT id,lastname FROM names ORDER BY id;
SELECT DISTINCT 2+id,lastname FROM names ORDER BY 2+id;
SELECT DISTINCT 2+id,firstname || 'abcd',lastname FROM names ORDER BY 2+id,lastname,firstname || 'abcd';
SELECT DISTINCT 2+id,firstname || 'abcd',lastname FROM names ORDER BY 3,2,1;
SELECT DISTINCT 2+id,firstname || 'abcd',lastname FROM names ORDER BY 3,2+id,2;
SELECT DISTINCT 2+id,firstname || 'abcd' AS col2,lastname FROM names ORDER BY 3,2+id,col2;
SELECT DISTINCT 2+id,firstname || 'abcd' AS col2,lastname FROM names ORDER BY 3,2+id,2;
SELECT DISTINCT id,firstname,lastname FROM names ORDER BY id,firstname,lastname,firstname;

SELECT DISTINCT (SELECT n1.lastname FROM names n1 LIMIT 1) FROM names ORDER BY 1;
SELECT DISTINCT (SELECT n1.lastname FROM names n1 LIMIT 1) FROM names ORDER BY (SELECT n1.lastname FROM names n1 LIMIT 1);
SELECT DISTINCT id,(SELECT n1.lastname FROM names n1 LIMIT 1),firstname FROM names ORDER BY id,(SELECT n1.lastname FROM names n1 LIMIT 1),firstname;

