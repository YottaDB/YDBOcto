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

-- TERR017 : OCTO482 : Test of "Missing FROM-clause entry" error

SELECT (SELECT * FROM (SELECT n1.id FROM names n1 WHERE n1.id = n2.id) n2);

SELECT (SELECT * FROM (SELECT * FROM (SELECT * FROM (SELECT DISTINCT n1.id FROM names n1 WHERE n4.id = n5.id) n2) n3) n4) FROM names n5;
SELECT (SELECT * FROM (SELECT * FROM (SELECT * FROM (SELECT DISTINCT n1.id FROM names n1 WHERE n3.id = n5.id) n2) n3) n4) FROM names n5;
SELECT (SELECT * FROM (SELECT * FROM (SELECT * FROM (SELECT DISTINCT n1.id FROM names n1 WHERE n2.id = n5.id) n2) n3) n4) FROM names n5;
SELECT (SELECT * FROM (SELECT * FROM (SELECT * FROM (SELECT DISTINCT n1.id FROM names n1 WHERE n3.id = n4.id) n2) n3) n4) FROM names n5;
SELECT (SELECT * FROM (SELECT * FROM (SELECT * FROM (SELECT DISTINCT n1.id FROM names n1 WHERE n2.id = n4.id) n2) n3) n4) FROM names n5;
SELECT (SELECT * FROM (SELECT * FROM (SELECT * FROM (SELECT DISTINCT n1.id FROM names n1 WHERE n1.id = n4.id) n2) n3) n4) FROM names n5;
SELECT (SELECT * FROM (SELECT * FROM (SELECT * FROM (SELECT DISTINCT n1.id FROM names n1 WHERE n2.id = n3.id) n2) n3) n4) FROM names n5;
SELECT (SELECT * FROM (SELECT * FROM (SELECT * FROM (SELECT DISTINCT n1.id FROM names n1 WHERE n1.id = n3.id) n2) n3) n4) FROM names n5;
SELECT (SELECT * FROM (SELECT * FROM (SELECT * FROM (SELECT DISTINCT n1.id FROM names n1 WHERE n1.id = n2.id) n2) n3) n4) FROM names n5;

SELECT * FROM names n1, (SELECT n1.id,n1.firstname,n1.lastname FROM names) n2;
SELECT * FROM names n1, (SELECT * FROM n1) n2;
SELECT * FROM (SELECT n2.id) n1, names n2;
SELECT * FROM names n1, (SELECT n1.id) n2;

SELECT * FROM (SELECT * FROM names n1, (SELECT n1.id,n1.firstname,n1.lastname FROM names) n2);
SELECT * FROM (SELECT * FROM names n1, (SELECT * FROM n1) n2);
SELECT * FROM (SELECT * FROM (SELECT n2.id) n1, names n2);
SELECT * FROM (SELECT * FROM names n1, (SELECT n1.id) n2);

SELECT * FROM names n0, (SELECT * FROM names n1, (SELECT n0.id) n2) n3;
SELECT (SELECT * FROM names n0, (SELECT * FROM names n1, (SELECT n0.id) n2) n3);

SELECT (SELECT n1.id FROM names n1, (SELECT n1.id) n2);
SELECT (SELECT n1.firstname FROM names n1, (SELECT n1.id) n2);
SELECT (SELECT n2.id FROM names n1, (SELECT n1.id) n2);

SELECT * FROM customers c, (SELECT c.customer_id) c2;
SELECT * FROM customers c inner join (SELECT c.customer_id) c2 ON c2.customer_id = c.customer_id;

SELECT * FROM (SELECT n3.id) n3;

