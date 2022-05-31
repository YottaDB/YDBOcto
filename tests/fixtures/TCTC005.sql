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

-- TCTC005 : OCTO772 : Test CHECK constraint shows up correctly in CREATE TABLE text definition
-- Test all possible CHECK constraint expressions (i.e. all "case" code paths in "src/emit_check_constraint.c")

create table tmp01 (id integer, check ((true is unknown) and (false is null)));
create table tmp02 (id integer, check (('abcd' != 'efgh') and (1 <= id) and (1.5 > abs(-2.35))));
create table tmp03 (id integer, check (NOT nullif(1.0, 1) is not null));
create table tmp04 (id integer, firstname varchar, check (greatest(firstname, 'abcd', NULL, 'zzz') is null));
create table tmp05 (id integer, firstname varchar, lastname varchar, check (least(firstname || 'efgh', lastname, firstname || lastname) is null));
create table tmp06 (id integer, lastname varchar, check (coalesce(NULL, lastname) is null));
create table tmp07 (id integer, lastname varchar, check (nullif(NULL, 2) is null));
create table tmp08 (id integer, check(case id when 2 then true end));
create table tmp09 (id integer, check(case id when 2 then true else false end));
create table tmp10 (id integer, check(case when id is null then true end));
create table tmp11 (id integer, check(case when id is null then true else false end));
create table tmp12 (id integer, check(id::boolean));
create table tmp13 (id integer, check(id::numeric(3) = 3.0));
create table tmp14 (id integer, check(id::numeric(4,1) != 4.5));
create table tmp15 (id integer, check(id::integer < 4));
create table tmp16 (id integer, check(id::varchar(10) > 'abcd'));
create table tmp17 (id integer, check(+id >= -2));
create table tmp18 (id integer, check(NOT id::boolean));
create table tmp19 (id integer, check(id + (id % 3) - (id * 2) <= (id / 20)));
create table tmp20 (id integer, check(id::boolean OR NOT id::boolean));
create table tmp21 (id integer, check(id::boolean AND NOT id::boolean));
create table tmp22 (lastname varchar, check(lastname ~ 'abcd'));
create table tmp23 (lastname varchar, check(lastname ~* 'abcd'));
create table tmp24 (lastname varchar, check((lastname like 'abcd%') or (lastname not like 'abcd%')));
create table tmp25 (lastname varchar, check((lastname ilike 'ab%cd') or (lastname not ilike 'ab%cd')));
create table tmp26 (lastname varchar, check((lastname similar to 'ab%cd') and (lastname not similar to 'ab%cd')));
create table tmp27 (id integer, check(id in (2,3)));
create table tmp28 (id integer, check(id not in (2,3)));

-- Note: The below queries will not work because they need sub-query usage for ANY/ALL operator.
-- But sub-queries are not supported with CHECK constraint. So just recording them here but disabling it.
-- -- create table tmp29 (id integer, check(id = any (2,3)));
-- -- create table tmp30 (id integer, check(id != any (2,3)));
-- -- create table tmp31 (id integer, check(id < any (2,3)));
-- -- create table tmp32 (id integer, check(id > any (2,3)));
-- -- create table tmp33 (id integer, check(id <= any (2,3)));
-- -- create table tmp34 (id integer, check(id >= any (2,3)));
-- -- create table tmp35 (id integer, check(id = all (2,3)));
-- -- create table tmp36 (id integer, check(id != all (2,3)));
-- -- create table tmp37 (id integer, check(id < all (2,3)));
-- -- create table tmp38 (id integer, check(id > all (2,3)));
-- -- create table tmp39 (id integer, check(id <= all (2,3)));
-- -- create table tmp40 (id integer, check(id >= all (2,3)));

-- Test CHECK column constraint using a column ("price" below) that is going to be defined in the table in a later line
create table tmp29 (product_no integer CHECK (price > product_no), name text, price numeric);

-- Test that CONSTRAINT name is accepted but ignored for NOT NULL (proved below because same constraint name is accepted
-- for a different constraint without errors like it normally would if it was a real constraint name collision).
create table tmp30 (id integer unique, name varchar constraint name1 not null, firstname varchar constraint name1 check (firstname is not NULL));

-- Test that \d tablename in Octo matches that in Postgres for the below test case
-- This is a case where the constraint name is derived from an empty column name.
-- At one point, Octo used to create a constraint named PRODUCTS__CHECK whereas we expect it to be PRODUCTS_CHECK
-- (i.e. no double underscore).
create table tmp31 (id integer, check (1 + 2 = 3), check (2 + 3 = 5));

-- Test that constraint name of length 63 bytes is allowed
create table tmp32 (id integer CONSTRAINT toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqrstu CHECK (id < 2));

-- Test that when an auto assigned name would collide with a user specified constraint name, the auto assigned name
-- would move on to the next number and try that until it finds a non-colliding name. In the below, the (price < 10)
-- check constraint would normally have gotten the name tmp33_price_check but since that name already exists it
-- would move on and then try tmp33_price_check1 but it finds that one taken as well and so moves on and then
-- assigns tmp33_price_check2 as the constraint name.
create table tmp33 (
	      product_no integer,
	      name text,
	      price numeric CONSTRAINT tmp33_price_check CHECK (price > 0) CONSTRAINT tmp33_price_check1 CHECK (price > 5)
	       CHECK (price < 10)
	  );

-- Test CHECK column constraint using another (but valid) column name automatically becomes a table level constraint
-- (indicated by the constraint name having just the table name and not any column name in it).
create table tmp34 (
	      product_no integer,
	      name text,
	      price numeric CHECK (price > product_no)
	  );

-- Test CHECK column constraint using a column that is not yet defined in the table but will be in a later line.
-- This should also become a table level constraint (indicated by the constraint name having just the table name
-- and not any column name in it).
create table tmp35 (
	      product_no integer CHECK (price > product_no),
	      name text,
	      price numeric
	  );

-- Test CHECK column constraint using a different column in its boolean condition expression.
-- It should be named after that column and not the current column (i.e. it should become that column's CHECK constraint).
-- In the below, the CHECK constraint is specified under the "name" column but uses "id" only. Therefore it becomes a column
-- level constraint of the "id" column as evidenced by the constraint named as "tmp36_id_check" (and not "tmp36_name_check").
create table tmp36 (id integer, name varchar check (id > 0));

-- Test a table definition that defines lots of column constraints but some correspond to a different column level constraint
-- and some correspond to a table level constraint. In the end, constraint names should be set correctly.
create table tmp37 (id integer, name varchar check (id > 0) check (name is not null) check ((name || id) = 'Zero2'));

-- Test that column level constraint that does not use any columns becomes a table level constraint.
-- Notice the (2 > 0) expression gets named as "tmp38_check" (and not "tmp38_name_check") because
-- it does not use "name" in the expression at all.
create table tmp38 (id integer, name varchar check (2 > 0) check (name is not null));

