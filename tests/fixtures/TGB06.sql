#################################################################
#								#
# Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TGB06 : OCTO55 : Edge case GROUP BY and/or HAVING queries that work in Postgres but do not work in Octo

--> Below should not error out even though subquery uses aggregate function on column from outer query
--> even though the aggregate function usage is inside a WHERE clause or FROM clause.
SELECT COUNT(id),(SELECT n2.id FROM names n2 WHERE n2.id = COUNT(n1.id)) FROM names n1;
SELECT (SELECT n3.id FROM names n3 ORDER BY COUNT(n1.id) LIMIT 1),(SELECT n2.id FROM names n2 WHERE n2.id = COUNT(n1.id) LIMIT 1) FROM names n1;
SELECT (SELECT n3.id FROM names n3 LEFT JOIN names n4 ON n4.id = COUNT(n1.id) ORDER BY COUNT(n1.id) LIMIT 1),(SELECT n2.id FROM names n2 WHERE n2.id = COUNT(n1.id) LIMIT 1) FROM names n1;
SELECT (SELECT n3.id FROM names n3 WHERE n3.id = COUNT(n1.id) LIMIT 1),(SELECT n2.id FROM names n2 ORDER BY COUNT(n1.id) LIMIT 1) FROM names n1;
SELECT (SELECT id FROM names n1 WHERE COUNT(n2.id) = 6 LIMIT 1),COUNT(n2.id) FROM names n2;

