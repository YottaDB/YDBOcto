#################################################################
#								#
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TOB20 : OCTO959 : Test that ORDER BY on a KEY column with LIMIT on a huge table is optimized

-- Test ORDER BY on INTEGER key column with or without LIMIT keyword
-- To test that ORDER BY with LIMIT is optimized, we just need to test that ORDER BY related M code is not emitted.
-- Therefore, all that the parent script needs to do is to check that [OrderBy] usages in the M code do not show up.
-- No need to do any LIMIT related M code checks.
select * from names order by id desc limit 1;  -- ORDER BY optimization should take effect
select * from names order by id limit 1;  -- ORDER BY optimization should take effect
select * from names order by id desc;  -- ORDER BY optimization should take effect
select * from names order by id;  -- ORDER BY optimization should take effect

-- Test ORDER BY on NUMERIC key column with or without LIMIT keyword
drop table if exists TOB20a;
create table TOB20a(id numeric primary key, firstname varchar, lastname varchar);
insert into TOB20a values (1.5, 'abcd', 'efgh');
insert into TOB20a values (-2.0, 'abcd', 'efgh');
insert into TOB20a values (-1.49, 'abcd', 'efgh');
insert into TOB20a values (-1.48, 'abcd', 'efgh');
select * from TOB20a order by id desc limit 2;  -- ORDER BY optimization should take effect
select * from TOB20a order by id desc;  -- ORDER BY optimization should take effect
select * from TOB20a order by id asc limit 2;  -- ORDER BY optimization should take effect
select * from TOB20a order by id asc;  -- ORDER BY optimization should take effect

-- Test ORDER BY on NUMERIC key column with or without LIMIT keyword
drop table if exists TOB20b;
create table TOB20b(id boolean primary key, firstname varchar, lastname varchar);
insert into TOB20b values (true, 'abcd', 'efgh');
insert into TOB20b values (false, 'abcd', 'efgh');
select * from TOB20b order by id desc limit 2;  -- ORDER BY optimization should take effect
select * from TOB20b order by id asc;  -- ORDER BY optimization should take effect

-- Test ORDER BY on VARCHAR/STRING key column with or without LIMIT keyword is NOT optimized.
-- Cannot optimize ORDER BY on a VARCHAR primary key column due to #397.
drop table if exists TOB20c;
create table TOB20c (zipcode varchar primary key, residents integer);
insert into TOB20c values ('22960', 20);
insert into TOB20c values ('02169', 10);
select * from TOB20c order by zipcode;
select * from TOB20c order by zipcode desc;
select * from TOB20c order by zipcode desc limit 2;
select * from TOB20c order by zipcode asc limit 2;

-- Test that ORDER BY optimization is disabled for DNF plans
select * from names where id = 3 OR id = 1 OR id = 2 order by id desc;
select * from names where id = 3 OR id = 1 OR id = 2 order by id asc;

-- Test that ORDER BY optimization is disabled for key fixing optimization using IN operator and a list of values
select * from names where firstname in ('Zero','Cereal')  order by id desc;
select * from names where firstname in ('Zero','Cereal')  order by id asc;

-- Test that ORDER BY optimization is enabled if only a subset of key columns is specified in ORDER BY as long as it is in order
select * from names n1, names n2 order by n1.id desc;
select * from names n1, names n2 order by n1.id asc;

-- Misc test of ORDER BY optimization involving more than 1 key column
select * from names n1, names n2 order by n1.id desc, n2.id asc;
select * from names n1, names n2 order by n1.id desc, n2.id desc;
select * from names n1, names n2 order by n1.id asc, n2.id desc;
select * from names n1, names n2 order by n1.id asc, n2.id asc;
select * from names n1, names n2 order by n1.id desc limit 2;
select * from names n1, names n2 order by n1.id asc limit 2;
select * from names n1, names n2 order by n1.id desc, n2.id asc limit 2;
select * from names n1, names n2 order by n1.id desc, n2.id desc limit 2;
select * from names n1, names n2 order by n1.id asc, n2.id desc limit 2;
select * from names n1, names n2 order by n1.id asc, n2.id asc limit 2;

-- Test that ORDER BY optimization is enabled even in the LP_KEY_FIX case as long as the
-- ORDER BY is done on the primary key column and not on the xref colum and in the same order as the FROM/JOIN.
select * from names where firstname = 'Zero' order by id desc;
select * from names n1, names n2, names n3 where n1.lastname is NULL and n2.firstname = 'Zero' order by n1.id desc, n2.id desc, n3.id asc;

-----------------------------------------------------------------------------------------------------
-- Below are tests to check "break" code paths in src/optimization_transforms/lp_optimize_order_by.c
-----------------------------------------------------------------------------------------------------

-- Test that ORDER BY optimization is disabled if key column of FROM table does not correspond to a SqlTable
select * from (select * from names) n1 order by id desc;
-- Also test VALUES clause disables ORDER BY optimization since there is no physical table/primary-key-column
select * from (values (1, 'abcd', 'efgh')) as n1(id,firstname,lastname) order by id desc;

-- Test that ORDER BY optimization is disabled if ORDER BY is not on a column alias
select * from names order by id + 2;

-- Test that ORDER BY optimization is disabled if ORDER BY column alias is not on a SqlColumn
select * from names n1 order by n1.*;

-- Test that ORDER BY optimization is disabled if ORDER BY column1 and KEY column1 correspond to different tables
select * from names n1, names n2 order by n2.id, n1.id;

-- Test that ORDER BY optimization is disabled if ORDER BY column and KEY column are in same table but not same column
select * from names n1 order by n1.firstname;

