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

-- TOB13 : OCTO412 : ORDER BY COLUMN NUM does not work if COLUMN NUM in select column list has a sub-query

SELECT (SELECT 6-n1.id) FROM names n1 ORDER BY 1;
SELECT (SELECT 6-n1.id) FROM names n1 where (n1.id < 2) ORDER BY 1;
SELECT (SELECT 6-n2.id FROM names n2 where n2.id = n1.id) FROM names n1 where (n1.id <= 2) ORDER BY 1;
SELECT (SELECT 6-n2.id FROM names n2 where n2.id = n1.id) FROM names n1 where (n1.id <= 2 OR n1.id >= 3) ORDER BY 1;
SELECT (SELECT 6-n1.id),(SELECT 5-n1.id),id FROM names n1 ORDER BY 1,2,n1.id;
SELECT (SELECT 6-n1.id),(SELECT 5-n1.id),id FROM names n1 ORDER BY 1,n1.id,2;
SELECT (SELECT 6-n1.id),(SELECT 5-n1.id),id FROM names n1 ORDER BY 2,1,n1.id;
SELECT (SELECT 6-n1.id),(SELECT 5-n1.id),id FROM names n1 ORDER BY 2,n1.id,1;
SELECT (SELECT 6-n1.id),(SELECT 5-n1.id),id FROM names n1 ORDER BY n1.id,1,2;
SELECT (SELECT 6-n1.id),(SELECT 5-n1.id),id FROM names n1 ORDER BY n1.id,2,1;
SELECT (SELECT 6-n1.id),(SELECT 5-n1.id),id FROM names n1 ORDER BY 1 desc,2,n1.id;
SELECT (SELECT 6-n1.id),(SELECT 5-n1.id),id FROM names n1 ORDER BY 1 desc,n1.id,2;
SELECT (SELECT 6-n1.id),(SELECT 5-n1.id),id FROM names n1 ORDER BY 2 desc,1,n1.id;
SELECT (SELECT 6-n1.id),(SELECT 5-n1.id),id FROM names n1 ORDER BY 2 desc,n1.id,1;
SELECT (SELECT 6-n1.id),(SELECT 5-n1.id),id FROM names n1 ORDER BY n1.id desc,1,2;
SELECT (SELECT 6-n1.id),(SELECT 5-n1.id),id FROM names n1 ORDER BY n1.id desc,2,1;

