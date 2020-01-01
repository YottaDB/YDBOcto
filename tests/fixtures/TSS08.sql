#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/OR its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TSS08 : OCTO413 : Sub query in SELECT column list issues incorrect <More than one row returned by a subquery> error

SELECT (SELECT n3.id FROM names n3 LEFT JOIN names n4 ON n4.id = n1.id ORDER BY n3.id limit 1) from names n1;
SELECT (SELECT n3.id FROM names n3 LEFT JOIN names n4 ON n4.id = n1.id ORDER BY ABS(n3.id-3) limit 1) from names n1;
SELECT (SELECT n3.id FROM names n3 LEFT JOIN names n4 ON n4.id = n1.id ORDER BY ABS(n3.id-4) limit 1) from names n1;
SELECT (SELECT n3.id FROM names n3 LEFT JOIN names n4 ON n4.id = n3.id ORDER BY ABS(n3.id-3),n3.id desc limit 1) from names n1;
SELECT (SELECT n3.firstname FROM names n3 LEFT JOIN names n4 ON n4.id = n1.id ORDER BY n3.firstname limit 1) from names n1;
SELECT (SELECT DISTINCT n3.firstname FROM names n3 LEFT JOIN names n4 ON n4.id = n1.id ORDER BY n3.firstname limit 1) from names n1;

-- Below queries did not issue any error but were tried out while fixing OCTO413 so are kept in the test

SELECT n3.id FROM names n3 LEFT JOIN names n4 ON n4.id = n3.id ORDER BY ABS(n3.id-3),n3.id desc;
SELECT (SELECT n3.id FROM names n3 LEFT JOIN names n4 ON n4.id = n1.id GROUP BY n3.id limit 1) from names n1;
SELECT (SELECT n3.firstname FROM names n3 LEFT JOIN names n4 ON n4.id = n1.id GROUP BY n3.firstname ORDER BY n3.firstname limit 1) from names n1;
SELECT (SELECT DISTINCT n3.firstname FROM names n3 LEFT JOIN names n4 ON n4.id = n1.id GROUP BY n3.firstname ORDER BY n3.firstname limit 1) from names n1;

