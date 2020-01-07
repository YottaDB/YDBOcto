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

-- TSS09 : OCTO428 : Assert failure when sub-queries and SET operations are used in same query

SELECT * FROM (SELECT * FROM names) n1 WHERE EXISTS (SELECT * FROM names UNION SELECT * FROM names);
SELECT * FROM (SELECT * FROM names) n1 WHERE EXISTS (SELECT * FROM names);
SELECT * FROM (SELECT * FROM names) n1 WHERE EXISTS (SELECT * FROM names n2 WHERE n2.id = 1);
SELECT * FROM (SELECT * FROM names) n1 WHERE EXISTS (SELECT * FROM names n2 WHERE n2.id = 1 OR n2.firstname = 'Zero');
SELECT * FROM (SELECT * FROM names) n1 WHERE id = ANY (SELECT n2.id FROM names n2 WHERE n2.id = 1 OR n2.firstname = 'Zero');
SELECT * FROM (SELECT * FROM names) n1 WHERE id = ALL (SELECT n2.id FROM names n2 WHERE n2.id = 1 OR n2.firstname = 'Zero');

SELECT * FROM (SELECT * FROM names UNION ALL SELECT * FROM names) n1 WHERE EXISTS (SELECT * FROM names UNION SELECT * FROM names);
SELECT * FROM (SELECT * FROM names UNION ALL SELECT * FROM names) n1 WHERE EXISTS (SELECT * FROM names);
SELECT * FROM (SELECT * FROM names UNION ALL SELECT * FROM names) n1 WHERE EXISTS (SELECT * FROM names n2 WHERE n2.id = 1);
SELECT * FROM (SELECT * FROM names UNION ALL SELECT * FROM names) n1 WHERE EXISTS (SELECT * FROM names n2 WHERE n2.id = 1 OR n2.firstname = 'Zero');
SELECT * FROM (SELECT * FROM names UNION ALL SELECT * FROM names) n1 WHERE id = ANY (SELECT n2.id FROM names n2 WHERE n2.id = 1 OR n2.firstname = 'Zero');
SELECT * FROM (SELECT * FROM names UNION ALL SELECT * FROM names) n1 WHERE id = ALL (SELECT n2.id FROM names n2 WHERE n2.id = 1 OR n2.firstname = 'Zero');

