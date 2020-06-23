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

-- TCJ09 : OCTO529 :  Query with lots of CROSS JOIN runs forever if WHERE clause contains lots of AND and = operators
-- This is tested extensively by the `test_sqllogic/select5` subtest. So no queries are added for this specifically.

-- TCJ10 : OCTO529 :  Test CROSS JOIN mixed with INNER JOIN cannot be freely reordered
-- The below query is not directly related to OCTO529 in that it does not take forever to run with or without the OCTO529 fixes
-- But with an interim state of the OCTO529 fixes, this query exposed an issue where a CROSS JOIN cannot be moved across an
-- INNER JOIN. The below two queries (which differ only in the placement of the CROSS JOIN) give different outputs
-- and therefore are not equivalent.
SELECT alias5.id, alias1.favoritePasta FROM names4  CROSS JOIN names4 AS alias1 CROSS JOIN (SELECT ALL alias2.id, alias2.pastaName FROM pastas alias2 ORDER BY alias2.id, alias2.pastaName) AS alias2 CROSS JOIN names4 AS alias4 INNER JOIN pastas AS alias5 ON (((alias4.id >= alias5.id)) OR (alias4.lastName <= alias5.pastaName) OR (alias4.lastName != alias5.pastaName) AND ((alias4.id = 2))) WHERE ((names4.id <= names4.id) OR NOT ('Cavatelli' < 'Cavatelli')) ORDER BY alias1.favoritePasta, alias5.id;
SELECT alias5.id, alias1.favoritePasta FROM names4 AS alias4 CROSS JOIN names4  CROSS JOIN names4 AS alias1 CROSS JOIN (SELECT ALL alias2.id, alias2.pastaName FROM pastas alias2 ORDER BY alias2.id, alias2.pastaName) AS alias2 INNER JOIN pastas AS alias5 ON (((alias4.id >= alias5.id)) OR (alias4.lastName <= alias5.pastaName) OR (alias4.lastName != alias5.pastaName) AND ((alias4.id = 2))) WHERE ((names4.id <= names4.id) OR NOT ('Cavatelli' < 'Cavatelli')) ORDER BY alias1.favoritePasta, alias5.id;

-- The below query is not directly related to OCTO529 in that it does not take forever to run with or without the OCTO529 fixes
-- But with an interim state of the OCTO529 fixes this query exposed an issue so is added to the automated test.
SELECT ALL * FROM names4  INNER JOIN names4 AS alias1 ON (((names4.favoritePasta > alias1.firstName))) LEFT JOIN (SELECT alias2.id, alias2.pastaName FROM pastas alias2) AS alias2 ON (((names4.id <= alias2.id))) RIGHT OUTER JOIN (SELECT ALL alias4.firstName, alias4.favoritePasta, alias4.id FROM names4 alias4) AS alias4 ON (((alias1.firstName = alias4.firstName) OR (alias1.favoritePasta >= alias4.firstName) OR (alias1.firstName < alias4.firstName)) AND NOT (alias1.id >= SOME (SELECT DISTINCT alias5.id FROM pastas alias5 ORDER BY alias5.id LIMIT 1))) CROSS JOIN (SELECT DISTINCT alias7.firstName FROM names4 alias7) AS alias7;

