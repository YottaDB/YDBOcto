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

-- All queries in this query file are invalid queries and generate an error both in Octo and Postgres.

SELECT DISTINCT firstname FROM names ORDER BY lastname;
SELECT DISTINCT firstname,lastname FROM names ORDER BY firstname,id;
-- A similar query as the below exists in TSD04_noerrors.sql and works fine there because it has `2+id AS id` there
-- which is a user-specified column name but here it only has `2+id` here and therefore takes on an implied column
-- alias of `id`. The ORDER BY id would only match user specified alias names and not implied alias names.
SELECT DISTINCT 2+id,lastname FROM names ORDER BY id;
SELECT DISTINCT 2+id,lastname FROM names ORDER BY 2+id,firstname;
SELECT DISTINCT 2+id,lastname FROM names ORDER BY 2+id,firstname,lastname;
SELECT DISTINCT 2+id,lastname FROM names ORDER BY 2+id,lastname,firstname || 'abcd';
SELECT DISTINCT 2+id,firstname || 'abcd',lastname FROM names ORDER BY 3,3+id,2;
SELECT DISTINCT 2+id,firstname || 'abcd',lastname FROM names ORDER BY 2+id,firstname || 'abcd',lastname,firstname;

-- Test that `2+id` in SELECT column list is not the same as `id+2` in ORDER BY column list
SELECT DISTINCT 2+id,firstname || 'abcd' AS col2,lastname FROM names ORDER BY 3,id+2,2;

SELECT DISTINCT id,(SELECT n1.lastname FROM names n1 LIMIT 1),firstname FROM names ORDER BY id,(SELECT DISTINCT n2.lastname FROM names n2 LIMIT 1),firstname;
SELECT DISTINCT id,(SELECT n1.lastname FROM names n1 LIMIT 1),firstname FROM names ORDER BY id,(SELECT n2.lastname FROM names n2 ORDER BY n2.lastname LIMIT 1),firstname;

-- The below query actually works in Octo but errors out in Postgres so is placed in this file instead of TSD04_noerrors.sql (as cross-check will fail)
SELECT DISTINCT id,(SELECT n1.lastname FROM names n1 LIMIT 1),firstname FROM names ORDER BY id,(SELECT n2.lastname FROM names n2 LIMIT 1),firstname;

