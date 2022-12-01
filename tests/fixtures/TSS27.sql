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

-- TSS27 : OCTO911 : Validate that SIG-11 is not seen when a subquery in the FROM clause has a select column with another subquery
select * from (select ((values(lastname))) from names) n1;
select * from (select (select firstname) from names) n1;
select * from (select (select 1 union select 1)n1 from names) n2;
select * from (select ((values(lastname))) || ((values(firstname))) from names) n1;
select * from (select ((values(lastname))) = 'Burn' from names) as n1;
