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

-- TC063 : OCTO633 : EXTRACT accepts SQL function calls

CREATE TABLE extractnames (
	id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30),  age INTEGER,
	fullname VARCHAR EXTRACT CONCAT(firstName, ' ', lastName),
	nameandnumber VARCHAR EXTRACT CONCAT(lastName, id::varchar)
) GLOBAL "^names(keys(""ID""))";

-- Select all columns from table with function call EXTRACT column
select * from extractnames;

-- No assert failure from successive references to function call EXTRACT column
select fullname from extractnames;
select fullname from extractnames;

-- Function referenced by EXTRACT column not dropped if containing table still exists
drop function concat(varchar, varchar, varchar);
select fullname from extractnames;

-- Function referenced by EXTRACT column can be dropped if containing table is dropped
drop table extractnames;
drop function concat(varchar, varchar, varchar);
select concat('no', 'more', 'concat');
CREATE FUNCTION CONCAT(VARCHAR, VARCHAR, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT; -- Recreate dropped function for use in below queries

-- Error issued for self-referential EXTRACT columns definitions
create table selfref (firstname varchar, lastname varchar, fullname varchar extract concat(fullname, lastname)) READONLY;
select * from selfref;

-- No error when referencing EXTRACT columns in tables with no PRIMARY KEY
create table tmp (id integer, abs_id integer extract ABS(id)) READONLY;
select abs_id from tmp;

-- No error when EXTRACT column references another EXTRACT column
drop table tmp;
create table tmp (id integer primary key, firstname varchar, lastname varchar, fullname1 varchar extract concat(firstname, lastname), fullname2 varchar extract concat(firstname, fullname1)) GLOBAL "^names";
select fullname1 from tmp where id = 3;
select fullname2 from tmp where id = 3;

-- Error issued for EXTRACT columns with cyclic dependencies on other EXTRACT columns
drop table tmp;
create table tmp (id integer primary key, firstname varchar, lastname varchar, fullname1 varchar extract concat(firstname, fullname2), fullname2 varchar extract concat(fullname1, lastname)) GLOBAL "^names";

-- Error issued when key column passed to `values(...)` or non-key column passed to `keys(...)`
CREATE TABLE tmp (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30), abs_id INTEGER EXTRACT "$$ABS^%ydboctosqlfunctions(values(""ID""))") GLOBAL "^names";
select * from tmp;
drop table tmp;
CREATE TABLE tmp (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30), abs_id INTEGER EXTRACT "$$ABS^%ydboctosqlfunctions(keys(""LASTNAME""))") GLOBAL "^names";
select * from tmp;
drop table tmp;

-- Prevent drop of function depended on by EXTRACT columns until all tables containing dependent columns are dropped
DROP FUNCTION IF EXISTS SAMEVALUE(INTEGER);
CREATE FUNCTION SAMEVALUE(INTEGER) RETURNS INTEGER AS $$samevalue^functions;
CREATE TABLE products1 (product_no integer, name text, price numeric CONSTRAINT name1 CHECK (samevalue(product_no) < 1000));
DROP FUNCTION SAMEVALUE(INTEGER);
CREATE TABLE products2 (product_no integer, name text, price numeric CONSTRAINT name2 CHECK (samevalue(product_no) < 1000));
DROP FUNCTION SAMEVALUE(INTEGER);
drop table if exists extractnames1;
CREATE TABLE extractnames1 (id INTEGER PRIMARY KEY, id1 INTEGER EXTRACT SAMEVALUE(id));
DROP FUNCTION SAMEVALUE(INTEGER);
drop table if exists extractnames2;
CREATE TABLE extractnames2 (id INTEGER PRIMARY KEY, id2 INTEGER EXTRACT SAMEVALUE(id));
DROP TABLE products1;
DROP FUNCTION SAMEVALUE(INTEGER);
DROP TABLE products2;
DROP FUNCTION SAMEVALUE(INTEGER);
drop table extractnames1;
DROP FUNCTION SAMEVALUE(INTEGER);
drop table extractnames2;
DROP FUNCTION SAMEVALUE(INTEGER);

drop table if exists extractnames1;
CREATE TABLE extractnames1 (
	id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30),  age INTEGER,
	fullname1 VARCHAR EXTRACT CONCAT(firstName, ' ', lastName)
) GLOBAL "^names1(keys(""ID""))";
drop function concat(varchar, varchar, varchar);
drop table if exists extractnames2;
CREATE TABLE extractnames2 (
	id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30),  age INTEGER,
	fullname2 VARCHAR EXTRACT CONCAT(firstName, ' ', lastName)
) GLOBAL "^names2(keys(""ID""))";
drop function concat(varchar, varchar, varchar);
select * from extractnames1;
select * from extractnames2;
drop table extractnames1;
drop function concat(varchar, varchar, varchar);
select * from extractnames2;
drop table extractnames2;
drop function concat(varchar, varchar, varchar);

-- Test that EXTRACT function parameters can be of different types.
-- This used to previously incorrectly issue a ERR_TYPE_MISMATCH error.
-- See https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/633#note_1348911984 for details.
create function myfunc1(varchar, integer, varchar) returns varchar as $$myfunc1^TC063;
create table tmp (id INTEGER PRIMARY KEY, firstName VARCHAR, lastName VARCHAR, fullname VARCHAR EXTRACT myfunc1(firstName, id, lastName)) GLOBAL "^names" readonly;
select * from tmp;
drop table tmp;
drop function myfunc1(varchar, integer, varchar);

-- Test that BOOLEAN literals are accepted as an EXTRACT function parameter
create function myfunc2(varchar, boolean, varchar) returns varchar as $$myfunc2^TC063;
create table tmp (id INTEGER PRIMARY KEY, firstName VARCHAR, lastName VARCHAR, fullname VARCHAR EXTRACT myfunc2(firstName, false, lastName)) GLOBAL "^names" readonly;
select * from tmp;
drop table tmp;

-- Test that type cast operators are accepted as an EXTRACT function parameter
create table tmp (id INTEGER PRIMARY KEY, firstName VARCHAR, lastName VARCHAR, fullname VARCHAR EXTRACT myfunc2(firstName, id::boolean, lastName)) GLOBAL "^names" readonly;
select * from tmp;
drop table tmp;
drop function myfunc2(varchar, boolean, varchar);

