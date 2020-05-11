#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TSS24 : OCTO368 : Using alias names for sub-query in select column list or FROM clause list fails with syntax error

-- Below are queries found in issue description as well as various comments of YDBOcto#368 on gitlab

select * from (select * from (select * from names union all select * from names) n0 union all (select * from names)) n1, names n2 limit 1;
select * from (select * from (select * from names union all select * from names) n0 union all (select * from names)) n1 inner join names n2 on n1.id = n2.id limit 1;
select * from (select * from (select * from names union all select * from names) n0 union all (select * from names)) n1 left join names n2 on n1.id = n2.id limit 1;
select * from (select * from (select * from names union all select * from names) n0 union all (select * from names)) n1 right join names n2 on n1.id = n2.id limit 1;
select * from (select * from (select * from names union all select * from names) n0 union all (select * from names)) n1 full join names n2 on n1.id = n2.id limit 1;
select * from names n1,(select * from (select * from names union all select * from names) n0 union all (select * from names)) n2 limit 1;
select * from names n1 inner join (select * from (select * from names union all select * from names) n0 union all (select * from names)) n2 on n1.id = n2.id limit 1;
select * from names n1 left join (select * from (select * from names union all select * from names) n0 union all (select * from names)) n2 on n1.id = n2.id limit 1;
select * from names n1 right join (select * from (select * from names union all select * from names) n0 union all (select * from names)) n2 on n1.id = n2.id limit 1;
select * from names n1 full join (select * from (select * from names union all select * from names) n0 union all (select * from names)) n2 on n1.id = n2.id limit 1;

select * from (select 1) n1;
select * from (select 1) n1;
select * from (select 1) n1, (select 2) n2;
select * from (select 1) as n1;
select * from (select 1) as n1;
select * from (select 1) as n1, (select 2) as n2;
select * from (select 1) n1, (select 2) as n2;
select * from (select 1) as n1, (select 2) n2;

select (select 1) n1;
select (select 1) n1, 2;
select (select 1) n1, (select 2) n2;
select (select 1) as n1;
select (select 1) as n1, 2;
select (select 1) as n1, (select 2) as n2;
select (select 1) n1, (select 2) as n2;
select (select 1) as n1, (select 2) n2;

-- Below are queries originally found as part of YDBOcto#350 but which work now even though #350 is still not fixed

select 2+(select * from ((select id from names) union (select id from names)) as n1 order by n1.id desc limit 1) from (select id from names) as n2;
select 2-(select * from ((select id from names) union (select 2::integer from names)) as n1 order by n1.id desc limit 1) from (select id from names) as n2;

