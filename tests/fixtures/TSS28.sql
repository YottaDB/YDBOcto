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

select (select * from (select n3.id from (select * from names) n1 limit 1) n2) from (select * from names) n3;
select (select * from (select n1.id) n2) from (select 1 as id) n1;
select (select * from (values(n1.id)) n2) from (select 1 as id) n1;
select (select * from (select n1.id union select n1.id) n2) from (select 1 as id) n1;
