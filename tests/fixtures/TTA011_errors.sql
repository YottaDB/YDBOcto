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

select 1 from names n1 group by n1.* order by n1.*, n1.firstname; -- Both Octo and Postgres issue error saying `n1.firstname` is not in GroupBy
select 1 from names n1 group by n1.firstname,n1.lastname order by n1.*; -- column n1.* not in group by
select 1 from names n1 group by n1.id,n1.firstname,n1.lastname order by n1.*; -- will work in postgres but not in Octo because of #675
select 1 from (select firstname,lastname from names) n1 group by n1.firstname,n1.lastname order by n1.*; -- demonstrates that without #675 the error seen in Octo is same as Postgres
select count(1) from names n1 order by n1.*; -- n1.* not in GROUP BY
select 1 from names n1 having 1<1 order by n1.*; -- n1.* not in GROUP BY

-- Following queries test SELECT DISTINCT behavior with Table.*
select distinct n1.* from names n1 order by n1.lastname,n1.*;
select distinct n1.* from names n1 order by n1.*;

-- Following queries validate that both queries only issue `ERR_GROUP_BY_OR_AGGREGATE_FUNCTION` error on n1.* column and not an expanded list of columns.
select 1 from names n1 order by count(*),n1.*;
select 1 from names n1 order by n1.*,count(*);
