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

-- TNJ06 : OCTO389 : NATURAL JOIN is mixed with INNER or OUTER JOIN in the same query

SELECT * FROM names n1 NATURAL JOIN names n2 LEFT  JOIN names n3 ON n1.id = n2.id;
SELECT * FROM names n1 NATURAL JOIN names n2 INNER JOIN names n3 ON n1.id = n2.id;
SELECT * FROM names n1 NATURAL JOIN names n2 RIGHT JOIN names n3 ON n1.id = n2.id;
SELECT n1.id,n2.id,n3.id FROM names n1 NATURAL JOIN names n2 INNER JOIN names n3 ON n2.id < n3.id;
SELECT n1.id,n2.id,n3.id FROM names n1 NATURAL JOIN names n2 LEFT  JOIN names n3 ON n2.id < n3.id;
SELECT n1.id,n2.id,n3.id FROM names n1 NATURAL JOIN names n2 RIGHT JOIN names n3 ON n2.id < n3.id;
SELECT * FROM names AS n1 NATURAL JOIN names AS n2 JOIN names AS n3 ON n2.id = n3.id;

