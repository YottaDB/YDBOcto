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

-- TUT007 : OCTO579 : Crosscheck of simple UPDATE queries in names database between Octo and Postgres

-- Populate temporary table TUT007 (for updates) from names table (read-only as it is used by various other tests)
drop table if exists TUT007;
create table TUT007 (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30));
insert into TUT007 (select * FROM names);

-- Test sub-query in SET clause works
update TUT007 set firstname = (select firstname || '#' from TUT007 ORDER BY 1 DESC limit 1) where firstname = 'Zero';
select * from TUT007;

-- Test OR operator in WHERE clause works
update TUT007 set firstname = firstname || '?' where firstname = 'Zero#' OR lastname = 'Cool';
select * from TUT007;
update TUT007 set firstname = firstname || '@' where firstname = 'Zero#?' OR lastname = 'Burn';
select * from TUT007;

-- Test type cast operator works (without that operator it would issue a ERR_TYPE_MISMATCH error, tested in TUT001.sql)
-- Also test that UPDATE without a WHERE clause works
update TUT007 set firstname = lastname, lastname = id::text;
select * from TUT007;

-- Test that VARCHAR(4) allows character strings less than or equal to 4 chars to be stored without space padding at end
drop table if exists TUT007;
create table TUT007 (column1 character(4));
-- Test that UPDATE works with NULL piece values too
insert into TUT007 VALUES (NULL);
insert into TUT007 VALUES ('a');
update TUT007 set column1 = column1 || 'B';
update TUT007 set column1 = column1 || 'C';
update TUT007 set column1 = column1 || 'D';
select * from TUT007;

-- Test that VARCHAR(4) allows 7 character strings to be stored if last 3 characters are spaces
delete from TUT007;
insert into TUT007 values ('a');
update TUT007 set column1 = column1 || 'BCD   ';
select * from TUT007;
delete from TUT007;
insert into TUT007 values ('a');
update TUT007 set column1 = column1 || 'bc    ';
select * from TUT007;

-- Test that an over length value when typecast to varchar(N) will be truncated to N characters without an error
delete from TUT007;
insert into TUT007 values ('|');
update TUT007 set column1 = column1 || 'abcd'::varchar(2) || '|';
select * from TUT007;
update TUT007 set column1 = 'abcdefg'::varchar(4);
select * from TUT007;

-- Test that excess digits to right of decimal point in NUMERIC(PRECISION,SCALE) column are discarded without an error
drop table if exists TUT007;
create table TUT007 (column1 NUMERIC(2,1));
insert into TUT007 values (0);
update TUT007 set column1 = 3;
select * from TUT007;
update TUT007 set column1 = -4.1;
select * from TUT007;
update TUT007 set column1 = 5.23;
select * from TUT007;
update TUT007 set column1 = -6.385;
select * from TUT007;
update TUT007 set column1 = -7.4999;
select * from TUT007;
update TUT007 set column1 = 9.499;
select * from TUT007;
update TUT007 set column1 = -9.501;
select * from TUT007;

-- Test that UPDATE works where column value is specified as NULL
drop table if exists TUT007;
create table TUT007 (id integer primary key, firstname varchar, lastname varchar);
insert into TUT007 (select * from names);
update TUT007 set firstname = NULL where lastname = 'Cool';
select * from TUT007;
select * from TUT007 where firstname is NULL;

-- Test that UPDATE works when primary key column is modified in increasing direction
-- a) Also test UPDATE with modifying only a key column
drop table if exists TUT007;
create table TUT007 (id INTEGER PRIMARY KEY, firstName VARCHAR(10), lastName VARCHAR(10));
insert into TUT007 select * from names;
update TUT007 set id = id + 10;
select * from TUT007;

-- b) Also test UPDATE with modifying a mix of key and non-key columns at the same time
drop table if exists TUT007;
create table TUT007 (id NUMERIC PRIMARY KEY, firstName VARCHAR(10), lastName VARCHAR(10));
insert into TUT007 select * from names;
update TUT007 set id = id + 0.5, firstname = lastname, lastname = firstname;
update TUT007 set id = id + 0.5, firstname = lastname, lastname = firstname;
select * from TUT007;

-- Test that UPDATE works when primary key column is modified in decreasing direction
drop table if exists TUT007;
create table TUT007 (id INTEGER PRIMARY KEY, firstName VARCHAR(10), lastName VARCHAR(10));
insert into TUT007 select * from names;
update TUT007 set id = id * -1;
select * from TUT007;

drop table if exists TUT007;
create table TUT007 (id NUMERIC PRIMARY KEY, firstName VARCHAR(10), lastName VARCHAR(10));
insert into TUT007 select * from names;
update TUT007 set id = id - 0.5, firstname = lastname, lastname = firstname;
update TUT007 set id = id - 0.5, firstname = lastname, lastname = firstname;
select * from TUT007;

-- Test that UPDATE does not issue a false ERR_DUPLICATE_KEY_VALUE error if key column is modified to same value
update TUT007 set id = id * 1;

-- Test UPDATE does not issue ZYSQLNULLNOTVALID error (happened in an interim version of the code) when non-key
-- columns are set to a NULL value (see https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/579#note_713264461)
drop table if exists TUT007;
create table TUT007 (id integer primary key, name varchar);
insert into TUT007 values (1, 'abcd');
update TUT007 set name = NULL;

