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

-- TSS11 : OCTO430 : <Problem resolving owner for deferred plan; undefined behavior> error when inherited sub-query column is used in another sub-query

SELECT * FROM (SELECT * FROM names UNION ALL SELECT * FROM names) n1 WHERE EXISTS (SELECT * FROM names n2 WHERE n2.id = n1.id);
SELECT * FROM (SELECT * FROM names UNION ALL SELECT * FROM names) n1 WHERE EXISTS (SELECT * FROM names n2 WHERE n1.id > n2.id UNION SELECT * FROM names n3 WHERE n1.firstname != n3.firstname);
SELECT * FROM (SELECT * FROM names UNION ALL SELECT * FROM names) n1 WHERE EXISTS (SELECT * FROM names n2 WHERE n1.id > n2.id);
SELECT * FROM (SELECT * FROM names UNION ALL SELECT * FROM names) n1 WHERE n1.id = ANY (SELECT n2.id FROM names n2 WHERE n2.id = n1.id);
SELECT * FROM (SELECT * FROM names UNION ALL SELECT * FROM names) n1 WHERE n1.id = ANY (SELECT n2.id FROM names n2 WHERE n1.id > n2.id UNION SELECT n3.id FROM names n3 WHERE n1.firstname != n3.firstname);
SELECT * FROM (SELECT * FROM names UNION ALL SELECT * FROM names) n1 WHERE n1.id = ANY (SELECT n2.id FROM names n2 WHERE n1.id > n2.id);
SELECT * FROM (SELECT * FROM names UNION ALL SELECT * FROM names) n1 WHERE n1.id = ALL (SELECT n2.id FROM names n2 WHERE n2.id = n1.id);
SELECT * FROM (SELECT * FROM names UNION ALL SELECT * FROM names) n1 WHERE n1.id = ALL (SELECT n2.id FROM names n2 WHERE n1.id > n2.id UNION SELECT n3.id FROM names n3 WHERE n1.firstname != n3.firstname);
SELECT * FROM (SELECT * FROM names UNION ALL SELECT * FROM names) n1 WHERE n1.id = ALL (SELECT n2.id FROM names n2 WHERE n1.id > n2.id);
SELECT * FROM (SELECT * FROM names UNION ALL SELECT * FROM names) n1 WHERE n1.id != ANY (SELECT n2.id FROM names n2 WHERE n2.id = n1.id);
SELECT * FROM (SELECT * FROM names UNION ALL SELECT * FROM names) n1 WHERE n1.id != ANY (SELECT n2.id FROM names n2 WHERE n1.id > n2.id UNION SELECT n3.id FROM names n3 WHERE n1.firstname != n3.firstname);
SELECT * FROM (SELECT * FROM names UNION ALL SELECT * FROM names) n1 WHERE n1.id != ANY (SELECT n2.id FROM names n2 WHERE n1.id > n2.id);
SELECT * FROM (SELECT * FROM names UNION ALL SELECT * FROM names) n1 WHERE n1.id != ALL (SELECT n2.id FROM names n2 WHERE n2.id = n1.id);
SELECT * FROM (SELECT * FROM names UNION ALL SELECT * FROM names) n1 WHERE n1.id != ALL (SELECT n2.id FROM names n2 WHERE n1.id > n2.id UNION SELECT n3.id FROM names n3 WHERE n1.firstname != n3.firstname);
SELECT * FROM (SELECT * FROM names UNION ALL SELECT * FROM names) n1 WHERE n1.id != ALL (SELECT n2.id FROM names n2 WHERE n1.id > n2.id);

