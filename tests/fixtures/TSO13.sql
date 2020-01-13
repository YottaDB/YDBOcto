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

-- TSO13 : OCTO440 : UNION/INTERSECT/EXCEPT does not issue column type mismatch error in some cases

--------------------------------------------------------
-- Queries that use the `names` database
--------------------------------------------------------

SELECT id, NULL, firstname FROM names UNION ALL SELECT id, firstname, NULL FROM names;
SELECT id, NULL, firstname FROM names UNION ALL SELECT id, id, NULL FROM names UNION ALL SELECT id, firstname, NULL from names;
SELECT id, id, firstname FROM names UNION ALL SELECT id, NULL, NULL FROM names UNION ALL SELECT id, firstname, NULL from names;
SELECT id, id, firstname FROM names UNION ALL SELECT id, firstname, NULL FROM names UNION ALL SELECT id, NULL, NULL from names;
SELECT id, id, firstname FROM names UNION ALL SELECT id, NULL, NULL FROM names UNION ALL SELECT id, NULL, NULL from names UNION ALL SELECT id, firstname, NULL from names;
SELECT id, id, firstname FROM names UNION ALL SELECT id, NULL, NULL FROM names UNION ALL SELECT id, firstname, NULL from names UNION ALL SELECT id, NULL, NULL from names;
SELECT id, id, firstname FROM names UNION ALL SELECT id, firstname, NULL FROM names UNION ALL SELECT id, NULL, NULL from names UNION ALL SELECT id, NULL, NULL from names;
SELECT id, firstname, firstname FROM names UNION ALL SELECT id, NULL, NULL FROM names UNION ALL SELECT id, NULL, NULL from names UNION ALL SELECT id, id, NULL from names;
SELECT id, firstname, firstname FROM names UNION ALL SELECT id, NULL, NULL FROM names UNION ALL SELECT id, id, NULL from names UNION ALL SELECT id, NULL, NULL from names;
SELECT id, firstname, firstname FROM names UNION ALL SELECT id, id, NULL FROM names UNION ALL SELECT id, NULL, NULL from names UNION ALL SELECT id, NULL, NULL from names;
SELECT id, NULL, firstname FROM names UNION ALL SELECT id, firstname, NULL FROM names UNION ALL SELECT id, NULL, NULL from names UNION ALL SELECT id, id, NULL from names;
SELECT id, NULL, firstname FROM names UNION ALL SELECT id, firstname, NULL FROM names UNION ALL SELECT id, id, NULL from names UNION ALL SELECT id, NULL, NULL from names;
SELECT id, NULL, firstname FROM names UNION ALL SELECT id, id, NULL FROM names UNION ALL SELECT id, firstname, NULL from names UNION ALL SELECT id, NULL, NULL from names;
SELECT id, NULL, firstname FROM names UNION ALL SELECT id, id, NULL FROM names UNION ALL SELECT id, NULL, NULL from names UNION ALL SELECT id, firstname, NULL from names;
SELECT id, NULL, firstname FROM names UNION ALL SELECT id, NULL, NULL FROM names UNION ALL SELECT id, id, NULL from names UNION ALL SELECT id, firstname, NULL from names;
SELECT id, NULL, firstname FROM names UNION ALL SELECT id, NULL, NULL FROM names UNION ALL SELECT id, firstname, NULL from names UNION ALL SELECT id, id, NULL from names;

SELECT * FROM names n1 INNER JOIN names n2 ON n1.id = n2.id
UNION ALL
(
  SELECT id, firstName, lastName, NULL, NULL, '' FROM names
  EXCEPT ALL
  SELECT n1.id, n1.firstName, n1.lastName, ''::text, NULL, NULL FROM names n1 INNER JOIN names n2 ON n1.id = n2.id
);

--------------------------------------------------------
-- Queries that use the `northwind` database
--------------------------------------------------------

-- Union of INTEGER and NUMERIC should not issue type mismatch error
SELECT * FROM (SELECT Price FROM Products UNION SELECT ProductID FROM Products) LIMIT 1;

-- Arithmetic operation of INTEGER and NUMERIC should not issue type mismatch error
SELECT Price + ProductID FROM Products LIMIT 1;

-- Union of NUMERIC and STRING/VARCHAR should issue type mismatch error
SELECT Price FROM Products UNION SELECT ProductName FROM Products LIMIT 1;

-- Arithmetic operation of NUMERIC and STRING/VARCHAR should issue type mismatch error
SELECT Price + ProductName FROM Products LIMIT 1;

-- Union of INTEGER and STRING/VARCHAR should issue type mismatch error
SELECT ProductID FROM Products UNION SELECT ProductName FROM Products LIMIT 1;

-- Arithmetic operation of INTEGER and STRING/VARCHAR should issue type mismatch error
SELECT ProductID + ProductName FROM Products LIMIT 1;

