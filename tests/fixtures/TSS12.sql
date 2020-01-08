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

-- TSS12 : OCTO431 : SIG-11 when inherited sub-query column is used in another sub-query that contains an OR operator

SELECT * FROM (SELECT * FROM names UNION ALL SELECT * FROM names) n1 WHERE EXISTS (SELECT * FROM names n2 WHERE n2.id = n1.id OR n2.firstname = n1.lastname);
SELECT * FROM (SELECT * FROM names UNION ALL SELECT * FROM names) n1 WHERE id = ANY (SELECT n2.id FROM names n2 WHERE n2.id = n1.id OR n2.firstname = n1.firstname);
SELECT * FROM (SELECT * FROM names UNION ALL SELECT * FROM names) n1 WHERE id = ALL (SELECT n2.id FROM names n2 WHERE n2.id = n1.id OR n2.firstname = n1.lastname);

