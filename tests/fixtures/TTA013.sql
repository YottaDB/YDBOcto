#################################################################
#								#
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

select n1.column1 = n2.column1 from (VALUES(1,'test'),(NULL,'test')) n1, (VALUES(NULL, 'test'), (1,'test'))n2;
select n1.column1 = n2.column1 from (VALUES(1,'test'),(NULL,'test')) n1, (VALUES(NULL, 'test'), (1,'test'))n2;

select n1.column1 IN (n2.column1) from (select 1,'test' as column1 union select NULL,'test') n1, (select NULL,'test' as column1 union select 1,'test') n2;
select n1.column1 IN (n2.column1) from (select 1,'test' as column1 union select NULL,'test') n1, (select NULL,'test' as column1 union select 1,'test') n2;

select n2.column1 >= NULL from (select 1 as column1 union select NULL) n1, (select NULL as column1 union select 'test') n2;
select n2.* >= NULL from (select 1 as column1 union select NULL) n1, (select NULL as column1 union select 'test') n2;
select NULL >= n2.* from (select 1 as column1 union select NULL) n1, (select NULL as column1 union select 'test') n2;
select NULL >= n2.* from (select NULL as column1) n2;
