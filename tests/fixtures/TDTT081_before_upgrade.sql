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
create table tdtt081 (id int primary key, dot1 time with time zone, dot2 time with time zone) global "^tdtt081" readonly;
-- Perform select on it
select * from tdtt081;
-- create table definition with date/time types in it and constraints
create table tdtt081_1 (id int, dot1 time with time zone, dot2 time with time zone);
-- Perform select on it
select * from tdtt081_1;
-- Add values to it
insert into tdtt081_1 values(1,time with time zone'01:01:01',time with time zone'01:01:01-05');
-- Perform select on it
select * from tdtt081_1;
-- create view definition with date/time types in it
create view tdtt081v as select time with time zone'01:01:01-05';
-- Perform select on it
select * from tdtt081v;
-- create function definition with date/time types as parameters and return types
create function tdtt081fd(time with time zone) returns time with time zone as $$samevalue^functions;
-- Perform select on it
select tdtt081fd(time with time zone'01:01:01-05');
-- constraint
create table tdtt081constraint(id integer primary key, dob time with time zone check(dob > time with time zone'02:00:00-05'));
insert into tdtt081constraint values(1,time with time zone'01:01:01-05');
insert into tdtt081constraint values(2,time with time zone'04:01:01-05');
select * from tdtt081constraint;
