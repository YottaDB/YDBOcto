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

-- https://gitlab.com/yottadb/dbms/ydbocto/-/merge_requests/1244#note_1388670999
drop view if exists v2739a;
drop view if exists v2739;
create view v2739 as select all first_name from ((select * from customers c1) union all (select * from customers c2) union all (select * from customers c3)) c group by first_name having exists (select 1 from orders group by order_id having count(c.customer_id) > 1);
create view v2739a as select * from v2739 union select * from v2739;
-- \d v2739a; -- Originally this command was used to identify the failure but it can be reproduced using select as well
select * from v2739a;
select * from v2739;
drop view if exists v2739a;
drop view if exists v2739;
