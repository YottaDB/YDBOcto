#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TABLENAME.ASTERISK in Group By
select firstname from names group by names.*;
select count(n1.*),n1.customer_id from customers as n1 group by n1.*;
select n1.customer_id from customers as n1 group by n1.*;
select count(n1.customer_id),n1.customer_id from customers as n1 group by n1.*;
select count(n1.*),* from customers as n1 group by n1.*;
select count(n1.*),n1.* from customers as n1 group by n1.*;
select n1.* from customers as n1 group by n1.*;
select count(n1.*),n1.name from ((select 1 as id, 'test' as name) union (select 2 as id, 'test' as name)) as n1 group by n1.*;
select n1.* from names n1 group by n1.*;
select count(n1.*),n1.id from names1col as n1 group by n1.*;
select count(n1.*),* from names1col as n1 group by n1.*;
select n1.* from names1col as n1 group by n1.*;
