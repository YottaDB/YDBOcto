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

-- TAF07 : OCTO617 : Test YDBOcto#617 optimization for various edge cases

-- Note: These are queries that use OCTO specific keywords and so are not kept in TAF07.sql (which is used against Postgres too).

-- Test that empty table works
create table TAF07octoa (id integer primary key, lastname varchar);
select min(lastname) is null from TAF07octoa;
select max(lastname) is null from TAF07octoa;

----------------------------------------------------------------------------------------------------
-- Note: The below READWRITE and READONLY tests are done on an empty table more so to verify from
-- the emitted M plan that the optimization is enabled or disabled as appropriate. The actual query
-- results are not checked as it is not considered worth the effort (requires adding data into the tables).
----------------------------------------------------------------------------------------------------
-- Test INTEGER typed key column on READWRITE table
create table TAF07octorw1 (id integer primary key, id2 integer);
select min(id2) is null from TAF07octorw1;
select max(id2) is null from TAF07octorw1;

-- Test NUMERIC typed key column on READWRITE table
create table TAF07octorw2 (id integer primary key, id2 numeric);
select min(id2) is null from TAF07octorw2;
select max(id2) is null from TAF07octorw2;

-- Test VARCHAR typed key column on READWRITE table
create table TAF07octorw3 (id integer primary key, id2 varchar);
select min(id2) is null from TAF07octorw3;
select max(id2) is null from TAF07octorw3;

-- Test BOOLEAN typed key column on READWRITE table
create table TAF07octorw4 (id integer primary key, id2 boolean);
select min(id2) is null from TAF07octorw4;
select max(id2) is null from TAF07octorw4;

-- Test DATE typed key column on READWRITE table
create table TAF07octorw5 (id integer primary key, id2 date);
select min(id2) is null from TAF07octorw5;
select max(id2) is null from TAF07octorw5;

-- Test TIME typed key column on READWRITE table
create table TAF07octorw6 (id integer primary key, id2 time);
select min(id2) is null from TAF07octorw6;
select max(id2) is null from TAF07octorw6;

-- Test TIME WITH TIME ZONE typed key column on READWRITE table
create table TAF07octorw7 (id integer primary key, id2 time with time zone);
select min(id2) is null from TAF07octorw7;
select max(id2) is null from TAF07octorw7;

-- Test TIMESTAMP typed key column on READWRITE table
create table TAF07octorw8 (id integer primary key, id2 timestamp);
select min(id2) is null from TAF07octorw8;
select max(id2) is null from TAF07octorw8;

-- Test TIMESTAMP WITH TIME ZONE typed key column on READWRITE table
create table TAF07octorw9 (id integer primary key, id2 timestamp with time zone);
select min(id2) is null from TAF07octorw9;
select max(id2) is null from TAF07octorw9;

-- Test INTEGER typed key column on READONLY table
create table TAF07octoro1 (id integer primary key, id2 integer) READONLY;
select min(id2) is null from TAF07octoro1;
select max(id2) is null from TAF07octoro1;

-- Test NUMERIC typed key column on READONLY table
create table TAF07octoro2 (id integer primary key, id2 numeric) READONLY;
select min(id2) is null from TAF07octoro2;
select max(id2) is null from TAF07octoro2;

-- Test VARCHAR typed key column on READONLY table
create table TAF07octoro3 (id integer primary key, id2 varchar) READONLY;
select min(id2) is null from TAF07octoro3;
select max(id2) is null from TAF07octoro3;

-- Test BOOLEAN typed key column on READONLY table
create table TAF07octoro4 (id integer primary key, id2 boolean) READONLY;
select min(id2) is null from TAF07octoro4;
select max(id2) is null from TAF07octoro4;

-- Test DATE typed key column on READONLY table
create table TAF07octoro5 (id integer primary key, id2 date) READONLY;
select min(id2) is null from TAF07octoro5;
select max(id2) is null from TAF07octoro5;

-- Test TIME typed key column on READONLY table
create table TAF07octoro6 (id integer primary key, id2 time) READONLY;
select min(id2) is null from TAF07octoro6;
select max(id2) is null from TAF07octoro6;

-- Test TIME WITH TIME ZONE typed key column on READONLY table
create table TAF07octoro7 (id integer primary key, id2 time with time zone) READONLY;
select min(id2) is null from TAF07octoro7;
select max(id2) is null from TAF07octoro7;

-- Test TIMESTAMP typed key column on READONLY table
create table TAF07octoro8 (id integer primary key, id2 timestamp) READONLY;
select min(id2) is null from TAF07octoro8;
select max(id2) is null from TAF07octoro8;

-- Test TIMESTAMP WITH TIME ZONE typed key column on READONLY table
create table TAF07octoro9 (id integer primary key, id2 timestamp with time zone) READONLY;
select min(id2) is null from TAF07octoro9;
select max(id2) is null from TAF07octoro9;

-- Test a query from customers data set using MAX/MIN on all columns (tests variety of column data types)
-- Test that for READONLY table integer/string etc. types are optimized
select max(order_id), min(order_id), max(customer_id), min(customer_id), max(order_amount), min(order_amount) from orders;
-- Test that for READONLY table date type is NOT optimized
select max(order_date), min(order_date) from orders;

-- Test case where all rows in table have NULL values for non-key column. Expect NULL return from MAX/MIN in this case.
create table TAF07octorw10 (id integer primary key, firstname varchar);
insert into TAF07octorw10 values (1, NULL);
select max(firstname) is null,min(firstname) is null from TAF07octorw10;

create table TAF07octorw11 (id integer primary key, firstname varchar);
insert into TAF07octorw11 values (1, NULL);
insert into TAF07octorw11 values (2, NULL);
select max(firstname) is null,min(firstname) is null from TAF07octorw11;

create table TAF07octorw12 (id integer primary key, id2 integer);
insert into TAF07octorw12 values (1, NULL);
insert into TAF07octorw12 values (2, NULL);
select max(id2) is null,min(id2) is null from TAF07octorw12;

create table TAF07octorw13 (id integer primary key, id2 integer);
insert into TAF07octorw13 values (1, NULL);
select max(id2) is null,min(id2) is null from TAF07octorw13;

-- Test that MAX/MIN never return NULL as long as there is at least one row with a non-NULL column value for the non-key column
create table TAF07octorw14 (id integer primary key, id2 integer);
insert into TAF07octorw14 values (1, NULL);
insert into TAF07octorw14 values (2, 3);
select max(id2),min(id2) from TAF07octorw14;
-- Test that query with aggregate function usage that is not MIN or MAX does not get optimized
select sum(id2) from TAF07octorw14;

create table TAF07octorw15 (id integer primary key, id2 varchar);
insert into TAF07octorw15 values (1, NULL);
insert into TAF07octorw15 values (2, 'abcd');
select max(id2),min(id2) from TAF07octorw15;

