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

-- TSS07 : OCTO352 : Incorrect results when sub-query is used in SELECT or ORDER BY column list

-- Sub-query in SELECT column list

SELECT firstname,(SELECT b.firstname FROM names AS b LIMIT 1) FROM names;
SELECT A.firstName, (SELECT B.firstName FROM NAMES LIMIT 1) FROM NAMES A INNER JOIN NAMES B on (A.firstName = B.firstName);
SELECT A.firstName, (SELECT C.firstName FROM NAMES AS C WHERE C.firstName = B.firstName LIMIT 1) FROM NAMES A INNER JOIN NAMES B on (A.firstName = B.firstName);
SELECT A.id,A.firstName,B.id,B.firstName,(SELECT C.firstName FROM NAMES AS C WHERE C.firstName = B.firstName LIMIT 1) FROM NAMES A INNER JOIN NAMES B on (A.firstName = B.firstName);
SELECT A.id,A.firstName,B.id,B.firstName,(SELECT C.firstName FROM NAMES AS C WHERE C.id = B.id) FROM NAMES A INNER JOIN NAMES B on (A.firstName = B.firstName);

-- Sub-query in ORDER BY column list

SELECT * FROM names n1 ORDER BY (SELECT n1.firstname);

