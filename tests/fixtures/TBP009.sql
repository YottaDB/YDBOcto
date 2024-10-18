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

-- TBP009 : OCTO1062 : Test SQL keywords accepted as table/column names for various commands

-- Try out TIMESTAMP keyword as table and or column name in various commands
create table test (timestamp timestamp);
select t.timestamp from test t;
select timestamp from test;
drop table test;

create table timestamp (timestamp timestamp);
select t.timestamp from timestamp t;
select timestamp from timestamp;
drop table timestamp;

drop table if exists timestamp;
create table if not exists timestamp (timestamp timestamp);
insert into timestamp values (NULL);
delete from timestamp where timestamp is NULL;
truncate timestamp;
drop table timestamp;

drop view if exists timestamp;
create view timestamp as select * from names;
drop view timestamp;

-- CREATE FUNCTION and DROP FUNCTION do not allow TIMESTAMP so use another keyword WHERE for testing.
create function timestamp (integer) returns integer as $$ABS^%ydboctosqlfunctions;
drop function timestamp (integer);

create function where (integer) returns integer as $$ABS^%ydboctosqlfunctions;
select where(-5);
drop function where (integer);

