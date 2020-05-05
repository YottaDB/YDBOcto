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

-- TERR019 : OCTO296 : Issue error when table on either side of NATURAL JOIN has duplicate columns with same name

-- names schema
SELECT * FROM names AS n1 join names AS n2 ON n1.id = n2.id NATURAL JOIN names AS n3;
SELECT * FROM names AS n1 NATURAL JOIN (SELECT * FROM names n2 INNER JOIN names n3 ON n2.id = n3.id) n4;
SELECT * FROM names AS n1 NATURAL JOIN names AS n2 JOIN names AS n3 ON n2.id = n3.id NATURAL JOIN names AS n4;
SELECT * FROM (SELECT * FROM names n1, names n2) n3 NATURAL JOIN names n4;
SELECT * FROM (SELECT * FROM names n1, names n2) n3 NATURAL JOIN names n4 WHERE n4.firstName = 'Zero';
SELECT * FROM names n3 NATURAL JOIN (SELECT * FROM names n1, names n2) n4;
SELECT * FROM names n3 NATURAL JOIN (SELECT * FROM names n1, names n2) n4 WHERE n3.firstName = 'Zero';

-- customers schema
SELECT c1.customer_id,o2.order_id,c3.customer_id FROM customers c1 LEFT JOIN orders o2 ON c1.customer_id = o2.customer_id NATURAL JOIN customers c3;

-- Test that error is issued if columns with same name on either side of NATURAL JOIN do not have same type
SELECT * from (SELECT id from names) n1 NATURAL JOIN (SELECT firstname AS id FROM names) n2;

-- Below are queries that do not issue error in Octo but are included in this file because they issue an error in Postgres
-- and so they need an explicit reference file instead of going through "run_query_in_octo_and_postgres_and_crosscheck"
SELECT * FROM names n1 NATURAL JOIN names n2 FULL  JOIN names n3 ON n1.id = n2.id;
SELECT n1.id,n2.id,n3.id FROM names n1 NATURAL JOIN names n2 FULL  JOIN names n3 ON n2.id < n3.id;

