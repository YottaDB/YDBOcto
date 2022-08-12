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

-- Test CREATE TABLE commands with a mix of table-level and column-level CHECK constraints.
-- This used to SIG-11/Assert-fail in a prior version of the code.
create table tmp39 (check (1 > 0), firstname varchar check (firstname > 'abcd'));
create table tmp40 (check (1 > 0), check (2 > 1), firstname varchar check (firstname > 'abcd'));
create table tmp41 (id integer, check (1 > 0), firstname varchar check (firstname > 'abcd'));

-- YDBOcto#582 : Test that UNIQUE constraint shows up correctly in CREATE TABLE text definition for all below queries
-- Additionally test the individual case described in comments before each query below

-- Test that column-level UNIQUE constraint can now specify a constraint name. And it shows in \d tablename.
create table tmp42 (id integer constraint uniq1 unique);

-- Test that table-level UNIQUE constraint can now specify a constraint name. And it shows in \d tablename.
-- Also test that table level UNIQUE constraint that specifies only one column is treated as a column level UNIQUE constraint
-- (i.e. UNIQUE shows up inside the "id" column in the table text definition)
create table tmp43 (id integer, constraint uniq1 unique (id));

-- Test table level UNIQUE constraint works if more than 1 column is specified
create table tmp44 (id1 integer, id2 integer, constraint uniq1 unique (id1, id2));

-- Test that \d tablename works even if table has a mix of CHECK and UNIQUE constraints
create table tmp45 (id1 integer, id2 integer, UNIQUE(id1, id2), CHECK (id1 > 2));

-- Test that only the first of multiple table-level unique constraints that correspond to the same set of columns gets
-- picked and the rest ignored. In the below example, the unique constraint "u2" gets ignored and "u1" gets picked.
create table tmp46 (id1 integer, constraint u1 unique (id1), constraint u2 unique (id1));

-- Test that if a mix of unnamed and named UNIQUE constraints are specified within one column, the first named constraint gets
-- picked and others get discarded.
create table tmp47 (id integer unique constraint uniq2 unique, name varchar constraint uniq1 unique);
create table tmp48 (id integer unique constraint uniq2 unique constraint uniq3 unique, name varchar constraint uniq1 unique);

-- Test that order of column names in UNIQUE constraint matters. That is (id1,id2) is different from (id2, id1).
-- The below example should create 2 UNIQUE constraints (instead of 1 which is what one would expect considering
-- the order of the columns should ideally not matter). Octo follows Postgres in this regard.
create table tmp49 (id1 integer, id2 integer, constraint u1 unique (id1, id2), constraint u2 unique (id2, id1));

-- Test that duplicate constraints get trimmed out even if multiple columns are involved in each constraint
create table tmp50 (id1 integer, id2 integer, constraint u1 unique (id1, id2), constraint u2 unique (id1, id2));

-- Test that if multiple table-level constraints are identical, the first NAMED table level constraint gets chosen.
-- In the below, the named constraint u2 gets chosen even though it is second after the first unnamed constraint.
-- Also u2 gets chosen ahead of u3 since both are named constraints and u2 is encountered first during the parse.
create table tmp51 (id1 integer, id2 integer, unique (id1, id2), constraint u2 unique (id1, id2), constraint u3 unique (id1, id2));

-- Test similar example as above except that this is with a table-level constraint that gets moved to a column-level constraint
-- because only 1 column is specified.
create table tmp52 (id1 integer, id2 integer, unique (id1), constraint u2 unique (id1), constraint u3 unique (id1));

-- Test that table-level UNIQUE constraint, using a column that will be defined later in the same CREATE TABLE, works fine
create table tmp53 (constraint uniq unique (id1), id1 integer);
create table tmp54 (id1 integer, constraint uniq unique (id2), id2 integer);
create table tmp55 (id1 integer, constraint uniq unique (id1, id2), id2 integer);

-- Test that multiple named UNIQUE constraints within one column cause the first one to be kept and remaining ones to be discarded.
-- In the below example, uniq2 named constraint in "id" column gets discarded because "uniq1" named constraint is already specified
-- and so there is no issue creating a constraint with that same name in "name" column.
create table tmp56 (id integer constraint uniq1 unique constraint uniq2 unique, name varchar constraint uniq2 unique);

-- Test that automatically assigned constraint name for UNIQUE is table_column1_column2_..._key.
create table tmp57 (id1 integer, id2 integer, unique (id1, id2), unique (id2, id1));

-- Test that UNIQUE constraints do NOT get trimmed out if one column list is a prefix of another column list.
-- In the below case `unique (id1, id2)` automatically implies `unique (id1, id2, id3)` so the second unique constraint
-- can be removed. But Postgres keeps both of them so Octo will also keep both of them.
create table tmp58 (id1 integer, id2 integer, id3 integer, unique (id1, id2), unique (id1, id2, id3));

-- Test auto generation of UNIQUE constraint name truncates table and/or column names as needed
-- Table name is short, Column name is long
create table tmp59 (toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqrstu integer unique);
-- Table name is long, Column name is short
create table tmp60toolongabcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopq (id integer unique);
-- Table name is long, Column name is long
create table tmp61toolongabcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopq (toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqrstu integer unique);
-- Table name is short, Column1 name is long, Column2 name is short
create table tmp62 (id1toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqr integer, id2 integer, unique (id1toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqr, id2));
-- Table name is short, Column1 name is long, Column2 name is long
create table tmp63 (id1toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqr integer, id2toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqr integer, unique (id1toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqr, id2toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqr));
-- Table name is long, Column1 name is short, Column2 name is short
create table tmp64toolongabcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopq (id1 integer, id2 integer, unique (id1, id2));
-- Table name is long, Column1 name is short, Column2 name is long
create table tmp65toolongabcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopq (id1 integer, id2toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqr integer, unique (id1, id2toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqr));
-- Table name is long, Column1 name is long, Column2 name is short
create table tmp66toolongabcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopq (id1toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqr integer, id2 integer, unique (id1toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqr, id2));
-- Table name is long, Column1 name is long, Column2 name is long
create table tmp67toolongabcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopq (id1toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqr integer, id2toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqr integer, unique (id1toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqr, id2toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqr));
-- Table name is long, Column1 name is long, Column2 name is long : Multiple UNIQUE constraints with conflicts in auto generated name
create table tmp68toolongabcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopq (toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqrid1 integer, toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqrid2 integer, unique (toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqrid1, toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqrid2), unique (toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqrid2, toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqrid1));
-- Table name is short, Column1 name is half long, Column2 name is half long
-- The last column name gets shortened as much as needed to get the unique name (i.e. `z2` became `z` in `...xyz_key`).
create table tmp69 (abcdefghijklmnopqrstuvwxyz1 integer, abcdefghijklmnopqrstuvwxyz2 integer, unique (abcdefghijklmnopqrstuvwxyz1, abcdefghijklmnopqrstuvwxyz2));

-- Test that when auto generated name already exists, new names are generated using "_key1", "_key2" syntax
create table tmp70 (abcdefghijklmnopqrstuvwxyz1 integer, abcdefghijklmnopqrstuvwxyz2 integer, abcdefghijklmnopqrstuvwxyz3 integer, unique (abcdefghijklmnopqrstuvwxyz1, abcdefghijklmnopqrstuvwxyz2), unique (abcdefghijklmnopqrstuvwxyz1, abcdefghijklmnopqrstuvwxyz3));
create table tmp71 (abcdefghijklmnopqrstuvwxyz1 integer, abcdefghijklmnopqrstuvwxyz2 integer, abcdefghijklmnopqrstuvwxyz3 integer, abcdefghijklmnopqrstuvwxyz4 integer, unique (abcdefghijklmnopqrstuvwxyz1, abcdefghijklmnopqrstuvwxyz2), unique (abcdefghijklmnopqrstuvwxyz1, abcdefghijklmnopqrstuvwxyz2, abcdefghijklmnopqrstuvwxyz3), unique (abcdefghijklmnopqrstuvwxyz1, abcdefghijklmnopqrstuvwxyz4, abcdefghijklmnopqrstuvwxyz2, abcdefghijklmnopqrstuvwxyz3));

-- Test auto generated UNIQUE constraint name collision with another named UNIQUE constraint name
create table tmp72 (id1 integer constraint tmp72_id2_key unique, id2 integer unique);

-- Test auto generated UNIQUE constraint name collision with another named CHECK constraint name
create table tmp73 (id1 integer constraint tmp73_id2_key CHECK (id1 > 1), id2 integer unique);

-- Test auto generated CHECK constraint name collision with another named UNIQUE constraint name
create table tmp74 (id1 integer constraint tmp74_id2_check unique, id2 integer CHECK (id2 > 1));

-- Test auto generated UNIQUE constraint name collision with CHECK constraint causes it to use "_key2"
create table tmp75toolongabcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopq (toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqrid1 integer, toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqrid2 integer, constraint tmp75toolongabcdefghijklmnopq_toolong1abcdefghijklmnopqrst_key1 CHECK (1 > 1), unique (toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqrid1, toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqrid2), unique (toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqrid2, toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqrid1));

