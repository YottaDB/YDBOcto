#################################################################
#								#
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TERR010 : Rocto prefixes query errors in its log

SELECT A.firstName, (SELECT B.firstName FROM NAMES LIMIT 1) FROM NAMES A INNER JOIN NAMES B on (A.firstName = B.firstName);
SELECT A.firstName, (SELECT C.firstName FROM NAMES AS C WHERE C.firstName = B.firstName LIMIT 1) FROM NAMES A INNER JOIN NAMES B on (A.firstName = B.firstName);
SELECT A.id,A.firstName,B.id,B.firstName,(SELECT C.firstName FROM NAMES AS C WHERE C.firstName = B.firstName LIMIT 1) FROM NAMES A INNER JOIN NAMES B on (A.firstName = B.firstName);
select * from names as n1 where 1::boolean;
select * from names as n1 where (select n2.id % 2 != 1 from names n2 where n1.id = n2.id);
select * from names as n1 where (n1.id != 0) AND (select n2.id != 1 from names n2 where n1.id = n2.id);
select * from names as n1 where NOT (select n2.id != 1 from names n2 where n1.id = n2.id);
