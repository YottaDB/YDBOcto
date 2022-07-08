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

select n1.column1 NOT IN (VALUES(NULL), (NULL)) from (VALUES('test1') ,('test2') ,('test3')) n1;
select n1.column1 = (VALUES(NULL)) from (VALUES('test1') ,('test2') ,('test3')) n1;
select n1.column1 =(VALUES(NULL)) from (VALUES('test1')) n1;
select (VALUES(NULL)) from (select firstname from names) n1;
select (VALUES(NULL)) from (select 1) n1;
select 1 from (select 1) n1 order by (VALUES(NULL));
