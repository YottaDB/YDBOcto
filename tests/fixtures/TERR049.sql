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

-- TERR049 : OCTO633 : Various EXTRACT error cases

DROP TABLE IF EXISTS extractnames;

-- Test aggregate functions disallowed in EXTRACT columns
CREATE TABLE extractnames (
    id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30),  age INTEGER,
    fullname VARCHAR EXTRACT SUM(id)
) GLOBAL "^names(keys(""ID""))";

-- Test subqueries disallowed in EXTRACT columns
CREATE TABLE extractnames (
    id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30),  age INTEGER,
    fullname VARCHAR EXTRACT (select * from names;)
) GLOBAL "^names(keys(""ID""))";

-- Test ERR_MISSING_FROM_ENTRY (EXTRACT function can only reference columns in table being created, not columns in other tables)
CREATE TABLE extractnames (
    id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30),  age INTEGER,
    fullname VARCHAR EXTRACT CONCAT(firstname, names.lastname)
) GLOBAL "^names(keys(""ID""))";

-- Test ERR_UNKNOWN_COLUMN_NAME
CREATE TABLE extractnames (
    id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30),  age INTEGER,
    fullname VARCHAR EXTRACT CONCAT(firstname, noname)
) GLOBAL "^names(keys(""ID""))";

-- Test TABLENAME.* not permitted as EXTRACT function arguments
CREATE TABLE extractnames (
    id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30),  age INTEGER,
    fullname VARCHAR EXTRACT CONCAT(extractnames.*, lastname)
) GLOBAL "^names(keys(""ID""))";

-- Test UNKNOWN function when incorrect type is passed, both with and without type coercion
CREATE TABLE extractnames (
    id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30),  age INTEGER,
    fullname VARCHAR EXTRACT CONCAT(1, lastname)
) GLOBAL "^names(keys(""ID""))";
CREATE TABLE extractnames (
    id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30),  age INTEGER,
    fullname VARCHAR EXTRACT CONCAT(firstname::integer, lastname)
) GLOBAL "^names(keys(""ID""))";

-- Error issued when column type doesn't match function call return type
drop table if exists tmp;
create table tmp (id integer primary key, fullname varchar extract abs(id));
select * from tmp;  -- Error: table `tmp` doesn't exist

-- Error issued when SQL expressions are used in EXTRACT function calls
drop table if exists tmp;
create table tmp (id varchar primary key, fullname1 varchar extract concat(id, id || 'abcd'));
select * from tmp;
