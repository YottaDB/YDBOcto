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

-- TSS10 : OCTO429 : Incorrect results when sub-queries with OR operators are used in WHERE clause of outer query with OR operators

SELECT * FROM (SELECT * FROM names WHERE id < 3) n1 WHERE id = 0 OR EXISTS (SELECT * FROM names n2 WHERE n2.id = 1 OR n2.firstname = 'Zero');
SELECT * FROM (SELECT * FROM names WHERE id < 3) n1 WHERE id = 0 OR id = ANY (SELECT n2.id FROM names n2 WHERE n2.id = 1 OR n2.firstname = 'Zero');
SELECT * FROM (SELECT * FROM names WHERE id < 3) n1 WHERE id = 0 OR id != ALL (SELECT n2.id FROM names n2 WHERE n2.id = 1 OR n2.firstname = 'Zero');
SELECT * FROM (SELECT * FROM names WHERE id = 1 OR firstname = 'Zero') n1 WHERE lastname = 'Burn' OR id = 0 OR EXISTS (SELECT * FROM names n2 WHERE n2.id = 1 OR n2.firstname = 'Zero');
SELECT * FROM (SELECT * FROM names WHERE id < 3) n1 WHERE lastname = 'Burn' OR id = 0 OR EXISTS (SELECT * FROM names n2 WHERE n2.id = 1 OR n2.firstname = 'Zero');
SELECT * FROM (SELECT * FROM names WHERE id < 3) n1 WHERE id = 0 OR NOT EXISTS (SELECT * FROM names n2 WHERE n2.id = 1 OR n2.firstname = 'Zero');
SELECT * FROM (SELECT * FROM names WHERE id < 3) n1 WHERE id = 0 OR id != ANY (SELECT n2.id FROM names n2 WHERE n2.id = 1 OR n2.firstname = 'Zero');
SELECT * FROM (SELECT * FROM names WHERE id < 3) n1 WHERE id = 0 OR id = ALL (SELECT n2.id FROM names n2 WHERE n2.id = 1 OR n2.firstname = 'Zero');

