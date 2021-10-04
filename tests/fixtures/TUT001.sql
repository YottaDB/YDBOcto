#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TU001 : OCTO579 : Test various errors in UPDATE

select '-- Test of ERR_TABLE_UNKNOWN_COLUMN_NAME error using a regular column';
update names set abcd = 2;

select '-- Test of ERR_TABLE_UNKNOWN_COLUMN_NAME error using a hidden key column';
create table tmp (name varchar);
update tmp set `%yo_keycol` = 2;
drop table tmp;

select '-- Test of ERR_MISSING_FROM_ENTRY';
update names n1 set firstname = 'abcd' where names.id = 2;

select '-- Test of ERR_TABLE_READONLY error against various catalog tables';
update octoOneRowTable set oid = oid + 1;
update pg_catalog.pg_namespace set nspname = nspname || 'abcd' where NULL != NULL;
update pg_catalog.pg_type set oid = oid + 1;
update pg_catalog.pg_class set oid = oid + 1;
update pg_catalog.pg_description set oid = oid + 1;
update information_schema.tables set oid = oid + 1 where NULL = NULL;
update pg_catalog.pg_proc set oid = oid + 1;
update pg_catalog.pg_attribute set oid = oid + 1;
update pg_catalog.pg_attrdef set oid = oid + 1;
update pg_catalog.pg_settings set name = 'efgh';
update pg_catalog.pg_database set oid = oid + 1;
update pg_catalog.pg_roles set oid = oid + 1;
update pg_catalog.pg_user set usersysid = usersysid + 1;

select '-- Test of ERR_TABLE_READONLY error in a table created with READONLY';
create table test1 (id INTEGER PRIMARY KEY, firstname VARCHAR) READONLY;
update test1 set firstname = 'efgh';
drop table test1;

select '-- Test same table created with READWRITE works fine with UPDATE';
create table test1 (id INTEGER PRIMARY KEY, firstname VARCHAR) READWRITE;
insert into test1 values (1, 'abcd');
update test1 set firstname = 'efgh';
select * from test1;
drop table test1;

select '-- Test of ERR_TABLE_READONLY error in a table created with READONLY implicitly assumed';
create table test1 (id INTEGER PRIMARY KEY, firstname VARCHAR DELIM "", lastname VARCHAR DELIM "");
update test1 set id = id + 1;
drop table test1;

select '-- Test of ERR_TYPE_NOT_COMPATIBLE error';
update names set id = id + 1 where firstname;

select '-- Test of ERR_TYPE_MISMATCH error';
update names set firstname = 2;
update names set firstname = lastname, id = lastname;
update names set firstname = lastname, lastname = id;
update names set firstname = (select id from names);

select '-- Test of various syntax errors (SET clause not specified etc.)';
update names where lastname = firstname;
update names set firstname != 'abcd' ;
update names set id * 2 = 3;
update names set where firstname = lastname;
update names set id = id + where firstname = lastname;

select '-- Test of ERR_UNKNOWN_TABLE error';
update abcd set id = 2;

select '-- Test of ERR_DUPLICATE_COLUMN error';
update names set id = 2, id = 3;
update names set id = 2, firstname = 'abcd', id = 3;
update names set id = 2, firstname = 'abcd', lastname = 'efgh', firstname = 'xyz';
update names set id = 2, firstname = 'abcd', firstname = 'efgh', id = 5;

select '-- Test case where ERR_DUPLICATE_COLUMN error is issued even if ERR_TABLE_UNKNOWN_COLUMN_NAME error also exists';
update names set id = 2, firstname = 'abcd', abcd = 4, id = 5;

select '-- Test case where ERR_TABLE_UNKNOWN_COLUMN_NAME error is issued even if ERR_DUPLICATE_COLUMN error also exists';
update names set firstname = 'abcd', abcd = 4, id = 4, id = 5;

select '-- Test of ERR_SUBQUERY_ONE_COLUMN error';
update names set firstname = (select firstname,lastname from names limit 1);

select '-- Test of ERR_SUBQUERY_MULTIPLE_ROWS error';
update names set firstname = (select firstname from names);

select '-- Test of simple ERR_VARCHAR_TOO_LONG error';
create table test1 (column1 VARCHAR(3));
insert into test1 values ('a');
update test1 set column1 = 'abcd';	-- 4 ascii characters
select * from test1;
update test1 set column1 = 'ＡＢＣＤ';	-- 4 utf-8 characters
select * from test1;
drop table test1;

select '-- Test of fancy ERR_VARCHAR_TOO_LONG error';
select '-- Test that VARCHAR(4) does not allow 7 character strings to be stored if not all last 3 characters are spaces.';
create table test1 (column1 VARCHAR(4));
insert into test1 values ('a');
select * from test1;
update test1 set column1 = 'abcd  e';
select * from test1;
update test1 set column1 = 'abcd e ';
select * from test1;
update test1 set column1 = 'abcde  ';
select * from test1;
update test1 set column1 = 'ＡＢＣＤ  Ｅ';
select * from test1;
update test1 set column1 = 'ＡＢＣＤ Ｅ ';
select * from test1;
update test1 set column1 = 'ＡＢＣＤＥ  ';
select * from test1;
drop table test1;

select '-- Test of fancier ERR_VARCHAR_TOO_LONG error';
select '-- Test that an over length value when typecast to varchar(N) will be truncated to N characters';
select '-- And that and an error will be issued if N is over length compared to the maximum column size';
create table test1 (column1 character(4));
insert into test1 values ('a');
update test1 set column1 = '|' || 'abcd'::varchar(3) || '|';
select * from test1;
update test1 set column1 = '|' || 'ＡＢＣＤ'::varchar(3) || '|';
select * from test1;
drop table test1;

select '-- Test of ERR_NUMERIC_OVERFLOW';
create table test1 (column1 NUMERIC(2,1));
insert into test1 values (0);
update test1 set column1 = 10;
update test1 set column1 = -10;
update test1 set column1 = 9.99;
update test1 set column1 = -9.99;
drop table test1;

select '-- Test of ERR_VARCHAR_TOO_LONG midway in the UPDATE';
create table test1 (column1 VARCHAR(3));
insert into test1 values ('a'), ('ab'), ('abc'), ('b');
update test1 set column1 = column1 || '#';
select '-- Verify that no rows are updated at the end';
select * from test1;
drop table test1;
select '-- Test that valid queries work fine after an UPDATE query that had an error midway';
select * from names where id = 4;

select '-- Test of ERR_DUPLICATE_KEY_VALUE on names database';
update names set id = id + 1;
update names set id = id + 1 where id = 4 OR id = 5;

select '-- Test ERR_DUPLICATE_KEY_VALUE error is not issued if OR operands are swapped';
update names set id = id + 1 where id = 5 OR id = 4;
select * from names;

select '-- Test of ERR_DUPLICATE_KEY_VALUE on composite database';
update composite set id0 = 1 where name = 'Name9';

select '-- Test of ERR_NULL_KEY_VALUE on names database';
update names set id = NULL;
update names set id = NULL where id = 5;

select '-- Test of ERR_NULL_KEY_VALUE on composite database';
update composite set id0 = NULL;
update composite set id3 = id3, id7 = NULL, id0 = id0;

