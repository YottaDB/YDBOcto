#################################################################
#								#
# Copyright (c) 2019 YottaDB LLC and/OR its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TSD02 : OCTO396 : LVUNDEF error when SELECT DISTINCT used in sub-query in EXISTS operator

SELECT id FROM names WHERE EXISTS (SELECT DISTINCT n1.id FROM names n1);

-- Try various other queries with DISTINCT usage in sub-queries and outer-queries

SELECT id FROM names WHERE EXISTS (SELECT DISTINCT * FROM names n1 WHERE n1.id = 1 OR n1.id = 2);
SELECT id FROM names WHERE EXISTS (SELECT DISTINCT n1.id FROM names n1 WHERE n1.id = 1 OR n1.id = 2);
SELECT * FROM names WHERE EXISTS (SELECT DISTINCT n1.id FROM names n1 WHERE n1.id = 1 OR n1.id = 2);
SELECT firstname FROM names WHERE EXISTS (SELECT DISTINCT n1.firstname FROM names n1);

SELECT DISTINCT * FROM names WHERE id = 1 OR id = 2;
SELECT DISTINCT id FROM names WHERE id = 1 OR id = 2;

SELECT id FROM names WHERE EXISTS (SELECT DISTINCT n1.id FROM names n1);
SELECT id FROM names WHERE EXISTS (SELECT DISTINCT * FROM names n1 WHERE n1.id = 1 OR n1.id = 2);
SELECT id FROM names WHERE EXISTS (SELECT DISTINCT n1.id FROM names n1 WHERE n1.id = 1 OR n1.id = 2);
SELECT * FROM names WHERE EXISTS (SELECT DISTINCT n1.id FROM names n1 WHERE n1.id = 1 OR n1.id = 2);
SELECT firstname FROM names WHERE EXISTS (SELECT DISTINCT n1.firstname FROM names n1);

SELECT DISTINCT id FROM names WHERE EXISTS (SELECT DISTINCT n1.id FROM names n1);
SELECT DISTINCT id FROM names WHERE EXISTS (SELECT DISTINCT * FROM names n1 WHERE n1.id = 1 OR n1.id = 2);
SELECT DISTINCT id FROM names WHERE EXISTS (SELECT DISTINCT n1.id FROM names n1 WHERE n1.id = 1 OR n1.id = 2);
SELECT DISTINCT * FROM names WHERE EXISTS (SELECT DISTINCT n1.id FROM names n1 WHERE n1.id = 1 OR n1.id = 2);
SELECT DISTINCT firstname FROM names WHERE EXISTS (SELECT DISTINCT n1.firstname FROM names n1);

