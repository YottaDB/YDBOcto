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

-- TBT03: Test of BOOLEAN type across various queries with errors OR different output between Octo & Postgres
CREATE FUNCTION DOLLARZWRITE(INTEGER) RETURNS VARCHAR AS $ZWRITE;

-- Test that unary operator on a boolean value issues error
select +id::boolean from names;
select -id::boolean from names;

-- Test that numeric type cast to boolean does not issue error (northwind database)
-- Note: Postgres issues an error for this but Octo does not since in M numeric and integer are the same.
select Price::boolean from Products;

-- Test that string type cast to boolean issues error
select firstname::boolean from names;
select DOLLARZWRITE(id)::boolean from names;

-- Test GROUP BY and AGGREGATE FUNCTIONs (COUNT, MIN, MAX, SUM, AVG etc.) using boolean column
-- MIN/MAX/SUM/AVG does not work with boolean types. Only COUNT works.
select min(mybool) from (select id=2 as mybool from names) n1;
select max(mybool) from (select id=2 as mybool from names) n1;
select sum(mybool) from (select id=2 as mybool from names) n1;
select avg(mybool) from (select id=2 as mybool from names) n1;

-- Test set operations on boolean and non-boolean type columns errors out
select id=2 from names union select id from names;
select id=2 from names union select firstname from names;
select Price=2.5 from Products union select Price from Products;

