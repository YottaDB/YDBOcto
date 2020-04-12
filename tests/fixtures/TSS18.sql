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

-- TSS18 : OCTO458 : Incorrect <Unknown column> error when column is inherited from a grandparent (not a parent) query

SELECT (SELECT * FROM (SELECT n1.id FROM names n1 WHERE n1.id = n3.id) n2) FROM names n3;
SELECT (SELECT n2.id FROM (SELECT n1.id FROM names n1 WHERE n1.id = n3.id) n2) FROM names n3;

SELECT (SELECT * FROM (SELECT n3.id from names n4 ORDER BY n3.id LIMIT 1) n2) FROM names n3;
SELECT (SELECT n3.id FROM (SELECT n3.id from names n4 ORDER BY n3.id LIMIT 1) n2 INNER JOIN names n1 ON n1.id = n3.id) FROM names n3;

SELECT (SELECT * FROM (SELECT * FROM (SELECT n1.id FROM names n1 WHERE n1.id = n4.id) n2) n3) FROM names n4;
SELECT (SELECT * FROM (SELECT n2.id FROM (SELECT n1.id FROM names n1 WHERE n1.id = n4.id) n2) n3) FROM names n4;
SELECT (SELECT n3.id FROM (SELECT n2.id FROM (SELECT n1.id FROM names n1 WHERE n1.id = n4.id) n2) n3) FROM names n4;
SELECT (SELECT n3.id FROM (SELECT * FROM (SELECT n1.id FROM names n1 WHERE n1.id = n4.id) n2) n3) FROM names n4;

SELECT (SELECT * FROM (SELECT n1.id FROM names n1 WHERE n1.id = n4.id UNION SELECT n2.id FROM names n2 WHERE n2.id = n4.id) n3) FROM names n4;

SELECT (SELECT * FROM (SELECT n1.id FROM names n1 WHERE n1.id = 5) n2) FROM names n3;
SELECT (SELECT * FROM (SELECT n1.id FROM names n1 WHERE n1.id > 3 ORDER BY 1 LIMIT 1) n2) FROM names n3;

-- The below query worked fine even before the OCTO458 code fixes but is included since it also has a grandparent query reference
SELECT * FROM names n1 WHERE n1.id IN (SELECT n2.id FROM names n2 WHERE n2.id IN (SELECT n3.id FROM names n3 WHERE n3.id = n1.id));

