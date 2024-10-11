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

-- TBT13 : OCTO867 : Test error scenarios when t/f literals are used

-- Test of ERR_TYPE_MISMATCH error
select * from names where 'f' = 1;
select * from names where true = (select 't');
select * from names where true = (select 't' union select 'f');
select 't' in (1);
select true in ('t', 'fa');
select * from names where 'f' = 1;
select * from names where 1 = 'f';
select 1 = case true when false then 't' when true then 'f' end;
create table TBT13 (id integer, CHECK (1 = case true when false then 't' when true then 'f' end));
select false in (values ('f'),('f'));
select NULLIF(false, 'abcd');
select NULLIF('abcd', false);
-- In the VALUES clause, Postgres treats 'f' as a STRING and not a BOOLEAN value. So Octo does the same
-- and issues a type mismatch error below.
select * from names where (firstname = 'Zero') in (values ('f'));

create table TBT13 (txt varchar);
insert into TBT13 values ('t'), (1);
drop table TBT13;

create table TBT13 (id integer, check ('t' in (1)));

create table TBT13 (id integer, check (true in ('t', 'fa')));

-- Test of ERR_MISTYPED_FUNCTION error when 't' or 'f' literal is used
select sum('t') from names;
select sum('f') from names;

-- Test of ERR_SETOPER_TYPE_MISMATCH error
select 't' union select false union select 'g';
select id=2 from names union values ('f');
select id from names union select 'f' from names;
select 'a','t' union select false,'f';

-- Test of ERR_CASE_BRANCH_TYPE_MISMATCH error
select case when id=1 then 't' else 1 end from names;
select 1 = case true when false then 2 when true then 'f' end;
select 1 = case true when false then 2 when true then 3 else 'f' end;
create table TBT13 (id integer, CHECK (case when id=1 then 't' else 1 end));
create table TBT13 (id integer, CHECK (1 = case true when false then 2 when true then 'f' end));
create table TBT13 (id integer, CHECK (1 = case true when false then 2 when true then 3 else 'f' end));

-- Test of ERR_CASE_VALUE_TYPE_MISMATCH error
select case 1 when 't' then 'MyZero' end;
select case 't' when 1 then 'MyZero' end;
select case 't' when true then 'abcd' end;
create table TBT13 (id integer, CHECK (case 1 when 't' then 'MyZero' end));
create table TBT13 (id integer, CHECK (case 't' when 1 then 'MyZero' end));
create table TBT13 (id integer, CHECK (case 't' when true then 'abcd' end));

-- Test of ERR_TYPE_NOT_COMPATIBLE error
select * from names where 't' AND 'g';
select * from names where ('f' or 'fat');

create table TBT13 (id integer, CHECK (case when id=1 then 't' else 'f' end));
insert into TBT13 values (1);
drop table TBT13;

-- Test of ERR_CHECK_CONSTRAINT_VIOLATION error
create table TBT13 (id integer, check ('f'));
\d TBT13;
insert into TBT13 values (1);
select * from TBT13;
drop table TBT13;

create table TBT13 (id varchar, check ('af' = concat(id, 'f')));
insert into TBT13 values ('a');
insert into TBT13 values ('b');
drop table TBT13;

create table TBT13 (id integer, check (true in ('f')));
insert into TBT13 values (4);
drop table TBT13;

create table TBT13 (id integer, check (true in ('f', 'f')));
insert into TBT13 values (5);
drop table TBT13;

create table TBT13 (id integer, check ('t' in (false, 'f')));
\d TBT13;
insert into TBT13 values (11);
drop table TBT13;

create table TBT13 (id integer, check (not 't'));
insert into TBT13 values (12);
drop table TBT13;

create table TBT13 (id integer, check (NULLIF('t', 'f') is NULL));
insert into TBT13 values (4);
drop table TBT13;

create table TBT13 (id integer, CHECK ('a' = case true when false then 't' when true then 'f' end));
insert into TBT13 values (1);
drop table TBT13;

create table TBT13 (id integer, CHECK ('a' = case true when false then 't' when true then 'f' end));
insert into TBT13 values (1);
drop table TBT13;

create table TBT13 (id integer, CHECK ('a' = case true when false then 't' when true then 'f' end));
insert into TBT13 values (1);
drop table TBT13;

create table TBT13 (id integer, CHECK ('a' = case true when false then 't' when true then 'f' else 'f' end));
insert into TBT13 values (1);
drop table TBT13;

create table TBT13 (id integer, CHECK ('a' = case true when false then '' when null then null when true then 'f' else 't' end));
insert into TBT13 values (1);
drop table TBT13;

create table TBT13 (id integer, CHECK ('a' = case true when false then '' when null then null when true then 'f' else null end));
insert into TBT13 values (1);
drop table TBT13;

create table TBT13 (id integer, CHECK ('a' = case when true then 't' when 't' then 't' when null then 'f' end));
insert into TBT13 values (1);
drop table TBT13;

-- Test of ERR_UNKNOWN_FUNCTION error
create table TBT13 (id varchar, check (fn(id, 'f')));

-- Test of ERR_INVALID_BOOLEAN_SYNTAX error
select not 'falsed' from names;
select * from names order by 'ff'::boolean;

-- Test of ERR_INVALID_INPUT_SYNTAX error
select -'t';

-- Test \d output shows 't' as TRUE inside CHECK constraint
create table TBT13 (id integer, check ('t' in (true, 'f')));
\d TBT13;
insert into TBT13 values (3);
select * from TBT13;
drop table TBT13;

-- Below are queries that work in Octo, but fail in Postgres. Not sure why.
-- They belong in the TBT12 subtest but cannot be included there because the crosscheck will fail.
-- So they are instead included in this TBT13 subtest.
select 't' union select 'f' union select true;

-- Test of various errors involving '' empty string literal (treated as NULL by Octo)
-- Octo differs from Postgres in '' handling in that it treats this as a NULL whereas Postgres treats this as an empty string
select null in (false, 'f', '');
select null in (true, 't', null, '');
select 't' in (true, null, 't', '');
select true = case true when false then '' end;
select true = case true when false then '' else 't' end;
select true = case true when false then '' when null then null end;
select true = case true when false then '' when null then null when true then true else 't' end;

create table TBT13 (txt boolean);
insert into TBT13 values ('t'), (true), ('f'), (false), (null), ('');
select * from TBT13;
drop table TBT13;

select NULLIF('', 'f');

create table TBT13 (id integer, check (null in (false, 'f', '')));
insert into TBT13 values (8);
select * from TBT13;
drop table TBT13;

create table TBT13 (id integer, check (null in (true, 't', null, '')));
insert into TBT13 values (9);
select * from TBT13;
drop table TBT13;

create table TBT13 (id integer, check ('t' in (true, null, 't', '')));
insert into TBT13 values (10);
select * from TBT13;
drop table TBT13;

drop table if exists tbt13;
create table tbt13 (id int, foo boolean);
insert into tbt13 values(9, 'y'), (10, 'abcd');
insert into tbt13 select 9, 'y' union select 10, 'abcd';
insert into tbt13 select 9, 'y' union select 10, 't'; -- errors as we don't know if 't' should be true or string from the set operation. Behavior is similar in Postgres. The same with a values clause is allowed in Octo as well as Postgres.
select * from tbt13;
create table tbt(foo varchar);
insert into tbt values('t'),('abcd'),('f');
insert into tbt13 select 1, 't' union select 2, * from tbt union select 3, true;
insert into tbt13 values((select 1), (select 't'));
insert into tbt13 select 1, 'abc' union select 2, false;
select * from tbt13;

