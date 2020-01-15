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

-- TBT02: Test of BOOLEAN type across various valid queries in `names` database

-- Test that integer type cast to boolean issues no error (names database)
select id::boolean from names;

-- Test that negative integer type cast to boolean issues no error (names database)
select (-id)::boolean from names;

-- Test ORDER BY a column that is a boolean type
select id,id = 2 from names order by 2;

-- Test GROUP BY and AGGREGATE FUNCTIONs (COUNT, MIN, MAX, SUM, AVG etc.) using boolean column
-- MIN/MAX/SUM/AVG does not work with boolean types. Only COUNT works.
select count(mybool) from (select id=2 as mybool from names) n1;

-- Test aggregate function queries as above but with typecast to see if that works
select count(mybool::integer) from (select id=2 as mybool from names) n1;
select min(mybool::integer) from (select id=2 as mybool from names) n1;
select max(mybool::integer) from (select id=2 as mybool from names) n1;
select sum(mybool::integer) from (select id=2 as mybool from names) n1;
select avg(mybool::integer * 6) from (select id=2 as mybool from names) n1;

-- Test boolean operations on boolean operands works fine
select (id = 0) or (id = 2) from names;

-- Test set operations on boolean type columns works fine
select id=2 from names union select id = 2 from names;

-- Test set operations on boolean and NULL type columns does not error out
select id=2 from names union all select null from names;

-- Test boolean literals in where clauses work fine
select * from names where true;
select * from names where false;

-- Test boolean literal usage in case statement
select case id = 1 when true then firstname else lastname end from names;
select case id = 1 when false then firstname else lastname end from names;

-- Test boolean 't' or 'f' printing in various cases
select firstname like 'Z%' from names;
select firstname not like 'Z%' from names;
select firstname similar to 'Z%' from names;
select firstname not similar to 'Z%' from names;
select firstname ~ 'Z.*$' from names;
select firstname !~ 'Z.*$' from names;
select n2.id,n2.id in (select n1.id from names n1 where n1.id > 2) from names n2;
select n2.id,n2.id not in (select n1.id from names n1 where n1.id > 2) from names n2;
select n2.id,n2.id = any (select n1.id from names n1 where n1.id > 2) from names n2;
select n2.id,n2.id != any (select n1.id from names n1 where n1.id > 2) from names n2;
select n2.id,n2.id < any (select n1.id from names n1 where n1.id > 2) from names n2;
select n2.id,n2.id > any (select n1.id from names n1 where n1.id > 2) from names n2;
select n2.id,n2.id <= any (select n1.id from names n1 where n1.id > 2) from names n2;
select n2.id,n2.id >= any (select n1.id from names n1 where n1.id > 2) from names n2;
select n2.id,n2.id = all (select n1.id from names n1 where n1.id > 2) from names n2;
select n2.id,n2.id != all (select n1.id from names n1 where n1.id > 2) from names n2;
select n2.id,n2.id < all (select n1.id from names n1 where n1.id > 2) from names n2;
select n2.id,n2.id > all (select n1.id from names n1 where n1.id > 2) from names n2;
select n2.id,n2.id <= all (select n1.id from names n1 where n1.id > 2) from names n2;
select n2.id,n2.id >= all (select n1.id from names n1 where n1.id > 2) from names n2;
select n1.id,n1.id = 2 from names n1;
select n1.id,n1.id != 2 from names n1;
select n1.id,n1.id < 2 from names n1;
select n1.id,n1.id <= 2 from names n1;
select n1.id,n1.id > 2 from names n1;
select n1.id,n1.id >= 2 from names n1;
select n1.id,(n1.id = 2) and (n1.id < 4) from names n1;
select n1.id,(n1.id != 2) and (n1.id < 4) from names n1;
select n1.id,(n1.id < 2) and (n1.id < 4) from names n1;
select n1.id,(n1.id <= 2) and (n1.id < 4) from names n1;
select n1.id,(n1.id > 2) and (n1.id < 4) from names n1;
select n1.id,(n1.id >= 2) and (n1.id < 4) from names n1;
select EXISTS (select * from names n1) and EXISTS (select * from names n2) from names n3;
select EXISTS (select * from names n1) and NOT EXISTS (select * from names n2) from names n3;

-- Test typecast to boolean. Occasionally test that `bool` is equivalent to `boolean`
select id::boolean,ABS(2) from names;
select id::bool,(ABS(2)::text || id::text) from names;
select 't'::boolean || id::text from names;
select id::text || 'f'::boolean from names;
select id::boolean,(ABS(2)::numeric + id) from names;
select id::boolean || id::text,ABS(2)::numeric * id from names;

