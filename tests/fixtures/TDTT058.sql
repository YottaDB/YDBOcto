#################################################################
#								#
# Copyright (c) 2023-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

select 1 group by date'2023-01-01'; -- Allowed in postgres
select 1 order by date'2023-01-01';
select date'2023-01-01' + time'01:01:01' as col1 group by col1;
select date'2023-01-01' + time'01:01:01' group by 1;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1404#note_1871948365
select order_date + time '10:03:54' as col1 from orders group by col1;
