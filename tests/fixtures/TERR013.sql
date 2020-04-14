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

-- TERR013 : Issue error when column name in query is ambiguous (due to multiple columns with same name)

-- OCTO364

-- Below query should work fine because there is no explicit column reference
SELECT * FROM (SELECT * FROM names n1, names n2) n3;

-- Below queries should not work fine because there is an explicit ambiguous column reference
SELECT * FROM (SELECT * FROM names n1, names n2) n3 WHERE firstName = 'Zero';
SELECT * FROM (SELECT * FROM names n1, names n2) n3 WHERE n3.firstName = 'Zero';

-- Below queries should not work fine because there is an implicit ambiguous column reference (due to the NATURAL JOIN)
SELECT * FROM (SELECT * FROM names n1, names n2) n3 NATURAL JOIN names n4;
SELECT * FROM (SELECT * FROM names n1, names n2) n3 NATURAL JOIN names n4 WHERE n4.firstName = 'Zero';
SELECT * FROM names n3 NATURAL JOIN (SELECT * FROM names n1, names n2) n4;
SELECT * FROM names n3 NATURAL JOIN (SELECT * FROM names n1, names n2) n4 WHERE n4.firstName = 'Zero';

-- Below queries should not work fine because there is implicit ambiguous column reference (due to the NATURAL JOIN)
--	as well as a duplicate table alias name (n3)
SELECT * FROM (SELECT * FROM names n1, names n2) n3 NATURAL JOIN names n3;
SELECT * FROM (SELECT * FROM names n1, names n2) n3 NATURAL JOIN names n3 WHERE n3.firstName = 'Zero';
SELECT * FROM names n3 NATURAL JOIN (SELECT * FROM names n1, names n2) n3;
SELECT * FROM names n3 NATURAL JOIN (SELECT * FROM names n1, names n2) n3 WHERE n3.firstName = 'Zero';

-- OCTO489

SELECT id AS col, firstname AS col from names ORDER BY col;
SELECT id AS id, firstname AS id from names ORDER BY id;
SELECT id FROM names n1 INNER JOIN names n2 ON n1.id < n2.id;
SELECT n1.id FROM names n1 INNER JOIN names n2 ON n1.id = (SELECT id);
SELECT a.lastname FROM names a LEFT JOIN names b ON id = 3;

