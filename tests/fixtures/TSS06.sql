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

-- TSS06 : OCTO405 : Incorrect results when sub-queries are used in the FROM clause and OR is used in the WHERE clause

SELECT * FROM (select * from names) n1 WHERE n1.id < 4 OR n1.id > 1;
SELECT n1.firstname FROM (select * from names) n1 WHERE n1.id < 4 OR n1.id > 1;
SELECT DISTINCT n1.firstname FROM (select * from names) n1 WHERE n1.id < 4 OR n1.id > 1;

-- Test UNION ALL in sub-query works fine with columns inherited from sub-queries and OR is used in the WHERE clause
SELECT n1.firstname FROM (select * from names UNION ALL select * from names) n1 WHERE n1.id < 4 OR n1.id > 1;
SELECT DISTINCT n1.firstname FROM (select * from names UNION ALL select * from names) n1 WHERE n1.id < 4 OR n1.id > 1;

