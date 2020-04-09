#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TAF01 : OCTO455 : AVG/MIN/MAX/SUM on empty table should return the NULL value if GROUP BY clause is absent

---------------------------------------------------
-- Queries without DISTINCT in aggregate functions
---------------------------------------------------

-- Test AVG/MIN/MAX/SUM without GROUP BY
SELECT AVG(id) FROM names WHERE id = 7;
SELECT AVG(id),COUNT(*) FROM names WHERE id = 7;
SELECT MIN(id) FROM names WHERE id = 7;
SELECT MIN(id),COUNT(*) FROM names WHERE id = 7;
SELECT MAX(id) FROM names WHERE id = 7;
SELECT MAX(id),COUNT(*) FROM names WHERE id = 7;
SELECT SUM(id) FROM names WHERE id = 7;
SELECT SUM(id),COUNT(*) FROM names WHERE id = 7;
SELECT COUNT(id) FROM names WHERE id = 7;
SELECT COUNT(id),COUNT(*) FROM names WHERE id = 7;

-- Test to verify that MIN/MAX/SUM/AVG indeed return NULL without GROUP BY
SELECT COUNT(*) FROM (SELECT MIN(id) AS nullid FROM names WHERE id = 7) AS nulltbl WHERE nullid IS NULL;
SELECT COUNT(*) FROM (SELECT MAX(id) AS nullid FROM names WHERE id = 7) AS nulltbl WHERE nullid IS NULL;
SELECT COUNT(*) FROM (SELECT SUM(id) AS nullid FROM names WHERE id = 7) AS nulltbl WHERE nullid IS NULL;
SELECT COUNT(*) FROM (SELECT AVG(id) AS nullid FROM names WHERE id = 7) AS nulltbl WHERE nullid IS NULL;

-- Test AVG/MIN/MAX/SUM with GROUP BY
SELECT AVG(id) FROM names WHERE id = 7 GROUP BY firstname;
SELECT AVG(id),COUNT(*) FROM names WHERE id = 7 GROUP BY firstname;
SELECT MIN(id) FROM names WHERE id = 7 GROUP BY firstname;
SELECT MIN(id),COUNT(*) FROM names WHERE id = 7 GROUP BY firstname;
SELECT MAX(id) FROM names WHERE id = 7 GROUP BY firstname;
SELECT MAX(id),COUNT(*) FROM names WHERE id = 7 GROUP BY firstname;
SELECT SUM(id) FROM names WHERE id = 7 GROUP BY firstname;
SELECT SUM(id),COUNT(*) FROM names WHERE id = 7 GROUP BY firstname;
SELECT COUNT(id) FROM names WHERE id = 7 GROUP BY firstname;
SELECT COUNT(id),COUNT(*) FROM names WHERE id = 7 GROUP BY firstname;

------------------------------------------------
-- Queries with DISTINCT in aggregate functions
------------------------------------------------

-- Test AVG/MIN/MAX/SUM without GROUP BY
SELECT AVG(DISTINCT id) FROM names WHERE id = 7;
SELECT AVG(DISTINCT id),COUNT(*) FROM names WHERE id = 7;
SELECT MIN(DISTINCT id) FROM names WHERE id = 7;
SELECT MIN(DISTINCT id),COUNT(*) FROM names WHERE id = 7;
SELECT MAX(DISTINCT id) FROM names WHERE id = 7;
SELECT MAX(DISTINCT id),COUNT(*) FROM names WHERE id = 7;
SELECT SUM(DISTINCT id) FROM names WHERE id = 7;
SELECT SUM(DISTINCT id),COUNT(*) FROM names WHERE id = 7;
SELECT COUNT(DISTINCT id) FROM names WHERE id = 7;
SELECT COUNT(DISTINCT id),COUNT(*) FROM names WHERE id = 7;

-- Test to verify that MIN/MAX/SUM/AVG indeed return NULL without GROUP BY
SELECT COUNT(*) FROM (SELECT MIN(DISTINCT id) AS nullid FROM names WHERE id = 7) AS nulltbl WHERE nullid IS NULL;
SELECT COUNT(*) FROM (SELECT MAX(DISTINCT id) AS nullid FROM names WHERE id = 7) AS nulltbl WHERE nullid IS NULL;
SELECT COUNT(*) FROM (SELECT SUM(DISTINCT id) AS nullid FROM names WHERE id = 7) AS nulltbl WHERE nullid IS NULL;
SELECT COUNT(*) FROM (SELECT AVG(DISTINCT id) AS nullid FROM names WHERE id = 7) AS nulltbl WHERE nullid IS NULL;

-- Test AVG/MIN/MAX/SUM with GROUP BY
SELECT AVG(DISTINCT id) FROM names WHERE id = 7 GROUP BY firstname;
SELECT AVG(DISTINCT id),COUNT(*) FROM names WHERE id = 7 GROUP BY firstname;
SELECT MIN(DISTINCT id) FROM names WHERE id = 7 GROUP BY firstname;
SELECT MIN(DISTINCT id),COUNT(*) FROM names WHERE id = 7 GROUP BY firstname;
SELECT MAX(DISTINCT id) FROM names WHERE id = 7 GROUP BY firstname;
SELECT MAX(DISTINCT id),COUNT(*) FROM names WHERE id = 7 GROUP BY firstname;
SELECT SUM(DISTINCT id) FROM names WHERE id = 7 GROUP BY firstname;
SELECT SUM(DISTINCT id),COUNT(*) FROM names WHERE id = 7 GROUP BY firstname;
SELECT COUNT(DISTINCT id) FROM names WHERE id = 7 GROUP BY firstname;
SELECT COUNT(DISTINCT id),COUNT(*) FROM names WHERE id = 7 GROUP BY firstname;

