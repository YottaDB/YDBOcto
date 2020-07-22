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

-- #346 Boolean expressions should be allowed in SELECT column list and WHERE clause
select ((1 != 2) != (1 != 2));
select 1 from names where ((NOT (1 != 2)) != (1 != 2));
select * from names as n1 where 1::boolean;
select * from names as n1 where (select n2.id % 2 != 1 from names n2 where n1.id = n2.id);
select * from names as n1 where (n1.id != 0) AND (select n2.id != 1 from names n2 where n1.id = n2.id);
select * from names as n1 where NOT (select n2.id != 1 from names n2 where n1.id = n2.id);
select * from names as n1 where NOT (select n2.id != 1 from names n2 where n1.id = n2.id);

-- Test that boolean literals can be used in comparisons
select true > false;
select true = false;
select true < false;
select true != false;

-- Test typecast from boolean to other types
-- Below should display 0 or 1
select id,(id = 2)::integer from names;
-- Below should display the strings `false` or `true`
select id,(id = 2)::varchar from names;
select id,(id = 2)::text from names;

-- Test that `::boolean` works
select id,(id = 2)::boolean from names;
-- Test that `::bool` works
select id,(id = 2)::bool from names;

-- Test boolean literals
select true = EXISTS (select * from names n1) from names n2;
select EXISTS (select * from names n1) from names n2;
select * from names where (true = (1 = 2));

-- Test typecast to boolean in WHERE clause
select * from names where 1::boolean;
-- Postgres allows 'string' || boolean; Octo allows the same for compatibility
select firstname || (id=2) from names;
