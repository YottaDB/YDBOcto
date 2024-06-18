#################################################################
#								#
# Copyright (c) 2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- create readonly table definition with date/time types in it and constraints
create table tdtt060 (id int primary key, dob date, dot time, dots timestamp, totstz timestamp with time zone) global "^tdtt060" readonly;
-- Perform select on it
select * from tdtt060;
-- create table definition with date/time types in it and constraints
create table tdtt060_1 (id int primary key, dob date, dot time, dots timestamp, totstz timestamp with time zone);
-- Perform select on it
select * from tdtt060_1;
-- Add values to it
insert into tdtt060_1 values(1,date'2023-01-01', time'01:01:01',timestamp'2023-01-01 01:01:01', timestamp with time zone'2023-01-01 01:01:01-05');
insert into tdtt060_1 values(2,date'2023-04-04', time'01:01:01',timestamp'2023-04-04 01:01:01', timestamp with time zone'2023-01-04 01:01:01-04');
-- Perform select on it
select * from tdtt060_1;
-- create view definition with date/time types in it
create view tdtt060v as select date'2023-01-01';
create view tdtt060v1 as select time'01:01:01';
create view tdtt060v2 as select timestamp'2023-01-01 01:01:01';
create view tdtt060v3 as select timestamp with time zone'2023-01-01 01:01:01-05';
create view tdtt060v4 as select timestamp with time zone'2023-04-01 01:01:01-04';
create view tdtt060v5 as select * from tdtt060;
create view tdtt060v6 as select * from tdtt060_1;
-- Perform select on it
select * from tdtt060v;
select * from tdtt060v1;
select * from tdtt060v2;
select * from tdtt060v3;
select * from tdtt060v4;
select * from tdtt060v5;
select * from tdtt060v6;
-- create function definition with date/time types as parameters and return types
create function tdtt060fd(date) returns date as $$samevalue^functions;
create function tdtt060ft(time) returns time as $$samevalue^functions;
create function tdtt060fts(timestamp) returns timestamp as $$samevalue^functions;
create function tdtt060ftstz(timestamp with time zone) returns timestamp with time zone as $$samevalue^functions;
-- Perform select on it
select tdtt060fd(date'2023-01-01');
select tdtt060ft(time'01:01:01');
select tdtt060fts(timestamp'2023-01-01 01:01:01');
select tdtt060ftstz(timestamp with time zone'2023-01-01 01:01:01-05');
select tdtt060ftstz(timestamp with time zone'2023-04-04 01:01:01-04');
-- extract test
-- TODO: Uncomment the following after extract issue with auto-upgrade is fixed
-- CREATE TABLE tdtt060extract (id integer PRIMARY KEY, dob numeric extract date_to_fileman(date'2023-01-01')) global "^tdtt060extract(keys(""id""))";
-- select * from tdtt060extract;
create table tdtt060constraint(id integer primary key, dob date check(dob > date'2023-01-01'));
insert into tdtt060constraint values(1,date'2023-02-01');
insert into tdtt060constraint values(2,date'2023-01-01');
select * from tdtt060constraint;

