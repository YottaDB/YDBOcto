#################################################################
#								#
# Copyright (c) 2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
(select firstname from names limit 1) union (select order_date from orders limit 1); -- error
create view v as (select firstname from names limit 1) union (select order_date from orders limit 1);
select * from v; -- error
(select '2023-01-01') union (select order_date from orders limit 1); -- valid
