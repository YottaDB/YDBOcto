#################################################################
#								#
# Copyright (c) 2022-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TC063 : OCTO633 : EXTRACT accepts SQL function calls
CREATE FUNCTION concatf(VARCHAR, VARCHAR, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concatf(VARCHAR, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;

CREATE TABLE extractnames (
	id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30),  age INTEGER,
	fullname VARCHAR EXTRACT CONCATF(firstName, ' ', lastName),
	nameandnumber VARCHAR EXTRACT CONCATF(lastName, id::varchar)
) GLOBAL "^names(keys(""id""))";

-- Select all columns from table with function call EXTRACT column
select * from extractnames;

-- No assert failure from successive references to function call EXTRACT column
select fullname from extractnames;
select fullname from extractnames;

-- Function referenced by EXTRACT column not dropped if containing table still exists
drop function concatf(varchar, varchar, varchar);
select fullname from extractnames;

-- Function referenced by EXTRACT column can be dropped if containing table is dropped
drop table extractnames;
drop function concatf(varchar, varchar, varchar);
select concatf('no', 'more', 'concatf');
CREATE FUNCTION CONCATF(VARCHAR, VARCHAR, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT; -- Recreate dropped function for use in below queries

-- Error issued for self-referential EXTRACT columns definitions
create table selfref (firstname varchar, lastname varchar, fullname varchar extract concatf(fullname, lastname)) READONLY;
select * from selfref;

-- No error when referencing EXTRACT columns in tables with no PRIMARY KEY
create table tmp (id integer, abs_id integer extract ABS(id)) READONLY;
select abs_id from tmp;

-- No error when EXTRACT column references another EXTRACT column
drop table tmp;
create table tmp (id integer primary key, firstname varchar, lastname varchar, fullname1 varchar extract concatf(firstname, lastname), fullname2 varchar extract concatf(firstname, fullname1)) GLOBAL "^names";
select fullname1 from tmp where id = 3;
select fullname2 from tmp where id = 3;

-- Error issued for EXTRACT columns with cyclic dependencies on other EXTRACT columns
drop table tmp;
create table tmp (id integer primary key, firstname varchar, lastname varchar, fullname1 varchar extract concatf(firstname, fullname2), fullname2 varchar extract concatf(fullname1, lastname)) GLOBAL "^names";

-- Error issued when key column passed to `values(...)` or non-key column passed to `keys(...)`
CREATE TABLE tmp (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30), abs_id INTEGER EXTRACT "$$ABS^%ydboctosqlfunctions(values(""id""))") GLOBAL "^names";
CREATE TABLE tmp (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30), abs_id INTEGER EXTRACT "$$ABS^%ydboctosqlfunctions(keys(""lastname""))") GLOBAL "^names";

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
	fullname1 VARCHAR EXTRACT CONCATF(firstName, ' ', lastName)
) GLOBAL "^names1(keys(""id""))";
drop function concatf(varchar, varchar, varchar);
drop table if exists extractnames2;
CREATE TABLE extractnames2 (
	id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30),  age INTEGER,
	fullname2 VARCHAR EXTRACT CONCATF(firstName, ' ', lastName)
) GLOBAL "^names2(keys(""id""))";
drop function concatf(varchar, varchar, varchar);
select * from extractnames1;
select * from extractnames2;
drop table extractnames1;
drop function concatf(varchar, varchar, varchar);
select * from extractnames2;
drop table extractnames2;
drop function concatf(varchar, varchar, varchar);

-- Test that EXTRACT function parameters can be of different types.
-- This used to previously incorrectly issue a ERR_TYPE_MISMATCH error.
-- See https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/633#note_1348911984 for details.
create function myfunc1(varchar, integer, varchar) returns varchar as $$myfunc^TC063;
create table tmp (id INTEGER PRIMARY KEY, firstName VARCHAR, lastName VARCHAR, fullname VARCHAR EXTRACT myfunc1(firstName, id, lastName)) GLOBAL "^names" readonly;
select * from tmp;
drop table tmp;

-- Test that BOOLEAN literals are accepted as an EXTRACT function parameter
create function myfunc2(varchar, boolean, varchar) returns varchar as $$myfunc^TC063;
create table tmp (id INTEGER PRIMARY KEY, firstName VARCHAR, lastName VARCHAR, fullname VARCHAR EXTRACT myfunc2(firstName, false, lastName)) GLOBAL "^names" readonly;
select * from tmp;
drop table tmp;

-- Test that type cast operators are accepted as an EXTRACT function parameter
create table tmp (id INTEGER PRIMARY KEY, firstName VARCHAR, lastName VARCHAR, fullname VARCHAR EXTRACT myfunc2(firstName, id::boolean, lastName)) GLOBAL "^names" readonly;
select * from tmp;
drop table tmp;

-- Test integer literals are accepted as an EXTRACT function parameter ("case INTEGER_LITERAL" in qualify_extract_function.c)
create function myfunc3(varchar, integer, varchar) returns varchar as $$myfunc^TC063;
create table tmp (
        id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30),
        fullname VARCHAR EXTRACT myfunc3(firstName, 99, lastName)
) GLOBAL "^names" READONLY;
select fullname from tmp;
drop table if exists tmp;

-- Test numeric literals are accepted as an EXTRACT function parameter ("case NUMERIC_LITERAL" in qualify_extract_function.c)
create function myfunc4(varchar, numeric, varchar) returns varchar as $$myfunc^TC063;
create table tmp (
        id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30),
        fullname VARCHAR EXTRACT myfunc4(firstName, 23.34, lastName)
) GLOBAL "^names" READONLY;
select fullname from tmp;
drop table if exists tmp;

-- Test NULL is accepted as an EXTRACT function parameter ("case NUL_VALUE" in qualify_extract_function.c)
create table tmp (
        id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30),
        fullname VARCHAR EXTRACT myfunc4(firstName, NULL, lastName)
) GLOBAL "^names" READONLY;
select fullname from tmp;
drop table if exists tmp;

-- Test empty single-quoted string is accepted as an EXTRACT function parameter ("case NUL_VALUE" in qualify_extract_function.c)
create table tmp (
        id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30),
        fullname VARCHAR EXTRACT myfunc4(firstName, '', lastName)
) GLOBAL "^names" READONLY;
select fullname from tmp;
drop table if exists tmp;

drop function myfunc1(varchar, integer, varchar);
drop function myfunc2(varchar, boolean, varchar);
drop function myfunc3(varchar, integer, varchar);
drop function myfunc4(varchar, numeric, varchar);

-- Test boolean string literal (e.g. 'f') is accepted as an EXTRACT function parameter
-- See comment in "case BOOLEAN_OR_STRING_LITERAL" in qualify_extract_function.c for how this is treated as a string literal
-- and not as a boolean literal.
create function concatf(VARCHAR, VARCHAR, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT; -- Recreate dropped function for use in below queries
create table fullnames (
        id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30),
        fullname VARCHAR EXTRACT CONCATF(firstName, 'f', lastName)
) GLOBAL "^names" READONLY;
select fullname from fullnames;
drop table if exists fullnames;

