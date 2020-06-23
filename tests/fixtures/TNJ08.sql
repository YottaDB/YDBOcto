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

-- TNJ08 : OCTO537 : Incorrect results when NATURAL JOIN is used with OUTER JOIN in same query

SELECT n1.id, n1.firstname, n2.id, n2.firstname, n3.id, n3.firstname FROM (SELECT 1 AS id, 'Name1' AS firstname) n1 NATURAL JOIN (SELECT 1 AS id, 'Name2' AS firstname) n2 LEFT JOIN (SELECT 2 AS id, 'Name3' AS firstname) n3 ON 1 = 1;
SELECT n1.id, n1.firstname, n2.id, n2.firstname, n3.id, n3.firstname FROM (SELECT 1 AS id, 'Name1' AS firstname) n1 NATURAL JOIN (SELECT 1 AS id, 'Name2' AS firstname) n2 LEFT JOIN (select 2 AS id, 'Name3' AS firstname) n3 ON 1 = 0;

