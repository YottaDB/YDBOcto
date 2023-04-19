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

select array(VALUES(1)) from (select * from names)n1 group by firstname;
select array(select 1) from (select * from names)n1 group by firstname;
select array(select 1 union select 1) from (select * from names)n1 group by firstname;
select array(values(1), (2), (3)) from (select * from names)n1;
select array(select 1 union select 2 union select 3) from (select * from names)n1;
select array(select n1.id limit 1) from (select * from names)n1;
