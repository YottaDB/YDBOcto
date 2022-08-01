#################################################################
#								#
# Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.	#
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

-- YDBOcto#770 : Test that PRIMARY KEY constraint shows up correctly in CREATE TABLE text definition for all below queries
-- Additionally test the individual case described in comments before each query below

-- Test unnamed PRIMARY KEY column constraint works with READONLY keyword
create table tmp76 (id integer PRIMARY KEY, firstname varchar, lastname varchar) READONLY GLOBAL "^names";
select count(*) from tmp76;

-- Test named PRIMARY KEY column constraint works with READONLY keyword
-- Also test that column level PRIMARY KEY constraint is now able to specify a constraint name.
create table tmp77 (id integer CONSTRAINT myPrimaryKey77 PRIMARY KEY, firstname varchar, lastname varchar) READONLY GLOBAL "^names";
select count(*) from tmp77;

-- Test unnamed PRIMARY KEY table constraint works with READONLY keyword
create table tmp78 (id integer, firstname varchar, lastname varchar, PRIMARY KEY (id)) READONLY GLOBAL "^names";
select count(*) from tmp78;

-- Test named PRIMARY KEY table constraint works with READONLY keyword
-- Also test that table level PRIMARY KEY constraint is now able to specify a constraint name.
create table tmp79 (id integer, firstname varchar, lastname varchar, CONSTRAINT myPrimaryKey79 PRIMARY KEY (id)) READONLY GLOBAL "^names";
select count(*) from tmp79;

-- Test \d lists a table-level PRIMARY KEY constraint in both cases below
create table tmp80 (id1 integer key num 0, id2 integer key num 1);
create table tmp81 (id1 integer, id2 integer, PRIMARY KEY (id1, id2));

-- Test column level PRIMARY KEY constraint and column level UNIQUE constraint specified for same column.
-- There should be just one PRIMARY KEY and no UNIQUE constraint in the \d output.
create table tmp82 (id integer primary key unique);

-- Test table level PRIMARY KEY constraint and table level UNIQUE constraint specified for same list of columns in same order.
-- There should be just one PRIMARY KEY and no UNIQUE constraint in the \d output.
create table tmp83 (id integer, primary key (id), unique (id));
create table tmp84 (id1 integer, id2 integer, primary key (id1, id2), unique (id1, id2));

-- Test table level PRIMARY KEY constraint and table level UNIQUE constraint specified for same list of columns but in
-- different order. There should be one PRIMARY KEY and one UNIQUE constraint in the \d output.
create table tmp85 (id1 integer, id2 integer, primary key (id1), unique (id2));
create table tmp86 (id1 integer, id2 integer, primary key (id1, id2), unique (id2, id1));

-- Test that if UNIQUE constraint is discarded because it is a duplicate of PRIMARY KEY constraint, any explicitly specified
-- name in the UNIQUE constraint is inherited by the PRIMARY KEY constraint if it does not have any explicitly specified name.
create table tmp87 (id integer primary key constraint uniq unique);
create table tmp88 (id integer constraint primkey88 primary key constraint abcd unique);
create table tmp89 (id integer, constraint primkey89 primary key (id), constraint uniq unique (id));
create table tmp90 (id integer, primary key (id), constraint uniq90 unique (id));
create table tmp91 (id1 integer, id2 integer, constraint primkey91 primary key (id1, id2), constraint uniq unique (id1, id2));
create table tmp92 (id1 integer, id2 integer, primary key (id1, id2), constraint uniq92 unique (id1, id2));

-- Test table-level UNIQUE constraint specified BEFORE table-level PRIMARY KEY constraint
create table tmp93 (unique (id2, id3), id1 integer, id2 integer, id3 integer, primary key (id2, id3));
create table tmp94 (constraint uniq94 unique (id2, id3), id1 integer, id2 integer, id3 integer, primary key (id2, id3));
create table tmp95 (unique (id2), id1 integer, id2 integer, primary key (id2));
create table tmp96 (constraint uniq96 unique (id2), id1 integer, id2 integer, primary key (id2));

-- Test that column-level PRIMARY KEY constraint can now specify a constraint name. And it shows in \d tablename.
create table tmp97 (id integer constraint pkey97 primary key);

-- Test that table-level PRIMARY KEY constraint can now specify a constraint name. And it shows in \d tablename.
-- Also test that table level PRIMARY KEY constraint that specifies only one column is treated as a column level PRIMARY KEY constraint
-- (i.e. PRIMARY KEY shows up inside the "id" column in the table text definition)
create table tmp98 (id integer, constraint pkey98 primary key (id));

-- Test table level PRIMARY KEY constraint works if more than 1 column is specified
create table tmp99 (id1 integer, id2 integer, constraint pkey99 primary key (id1, id2));

-- Test that \d tablename works even if table has a mix of CHECK, UNIQUE and PRIMARY KEY constraints
create table tmp100 (id1 integer, id2 integer, UNIQUE(id1, id2), CHECK (id1 > 2), constraint pkay100 primary key (id2));

-- Test that table-level PRIMARY KEY constraint, using a column that will be defined later in the same CREATE TABLE, works fine
create table tmp101 (constraint pkey101 primary key (id1), id1 integer);
create table tmp102 (id1 integer, constraint pkey102 primary key (id2), id2 integer);
create table tmp103 (id1 integer, constraint pkey103 primary key (id1, id2), id2 integer);

-- Test that automatically assigned constraint name for PRIMARY KEY is table_column1_column2_..._pkey.
create table tmp104 (id1 integer, id2 integer, primary key (id1, id2));

-- Test that automatically assigned constraint name for PRIMARY KEY ensures uniqueness across ALL existing tables in Octo
-- The table names in the below 3 lines are 63-bytes long and differ only in the last letter. And the column names are identical.
-- In that case, auto assigning a PRIMARY KEY constraint name by appending "_key" to the table and column names is bound to
-- generate the same constraint name for all the below tables IF the auto generation logic did not take this into account.
-- But it does take this into account and so we expect \d to show 3 different constraint names, the first one with a "_PKEY"
-- suffix and the second one with a "_PKEY1" suffix and the third one with a "_PKEY2" suffix.
create table tmp105toolongabcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnop (id integer primary key);
create table tmp105toolongabcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnoq (id integer primary key);
create table tmp105toolongabcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnor (id integer primary key);

-- Test auto generation of PRIMARY KEY constraint name truncates table and/or column names as needed
-- Table name is short, Column name is long
create table tmp106 (toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqrstu integer primary key);
-- Table name is long, Column name is short
create table tmp107toolongabcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnop (id integer primary key);
-- Table name is long, Column name is long
create table tmp108toolongabcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnop (toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqrstu integer primary key);
-- Table name is short, Column1 name is long, Column2 name is short
create table tmp109 (id1toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqr integer, id2 integer, primary key (id1toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqr, id2));
-- Table name is short, Column1 name is long, Column2 name is long
create table tmp110 (id1toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqr integer, id2toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqr integer, primary key (id1toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqr, id2toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqr));
-- Table name is long, Column1 name is short, Column2 name is short
create table tmp111toolongabcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnop (id1 integer, id2 integer, primary key (id1, id2));
-- Table name is long, Column1 name is short, Column2 name is long
create table tmp112toolongabcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnop (id1 integer, id2toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqr integer, primary key (id1, id2toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqr));
-- Table name is long, Column1 name is long, Column2 name is short
create table tmp113toolongabcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnop (id1toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqr integer, id2 integer, primary key (id1toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqr, id2));
-- Table name is long, Column1 name is long, Column2 name is long
create table tmp114toolongabcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnop (id1toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqr integer, id2toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqr integer, primary key (id1toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqr, id2toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqr));

-- Test auto generated PRIMARY KEY constraint name collision with another named UNIQUE constraint name
create table tmp115 (id1 integer constraint tmp115_pkey unique, id2 integer primary key);

-- Test auto generated UNIQUE KEY constraint name collision with another named PRIMARY KEY constraint name
create table tmp116 (id1 integer constraint tmp116_id2_key primary key, id2 integer unique);

-- Test auto generated PRIMARY KEY constraint name collision with another named CHECK constraint name
create table tmp117 (id1 integer constraint tmp117_pkey CHECK (id1 > 1), id2 integer primary key);

-- Test auto generated CHECK constraint name collision with another named PRIMARY KEY constraint name
create table tmp118 (id1 integer constraint tmp118_id1_check primary key, id2 integer CHECK (id1 > 1));

-- Test auto generated PRIMARY KEY constraint name collision with another named UNIQUE and CHECK constraint name
create table tmp119 (id2 integer constraint tmp119_pkey CHECK (id2 > 1), id3 integer constraint tmp119_pkey1 unique, id1 integer primary key);

-- Test auto generated UNIQUE constraint name collision with another named PRIMARY KEY and CHECK constraint name
create table tmp120 (id2 integer constraint tmp120_id1_key CHECK (id2 > 1), id3 integer constraint tmp120_id1_key1 primary key, id1 integer unique);

-- Test auto generated CHECK constraint name collision with another named PRIMARY KEY and UNIQUE constraint name
create table tmp121 (id1 integer constraint tmp121_id3_check primary key, id2 integer constraint tmp121_id3_check1 unique, id3 integer CHECK (id3 > 1));

-- Test constraints are double-quoted and do not fail any asserts, see https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1182#note_1290235104 for details
create table tmp122 (id integer constraint c CHECK (id > 1));
