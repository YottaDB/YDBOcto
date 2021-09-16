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

-- TDFT03 : OCTO54 : Test various errors in DELETE FROM

select '-- Test of ERR_UNKNOWN_COLUMN_NAME';
delete from names where abcd = 2;

select '-- Test of ERR_MISSING_FROM_ENTRY';
delete from names n1 where names.id = 2;

select '-- Test of ERR_TABLE_READONLY error against various catalog tables';
delete from octoOneRowTable;
delete from pg_catalog.pg_namespace where NULL != NULL;
delete from pg_catalog.pg_type;
delete from pg_catalog.pg_class;
delete from pg_catalog.pg_description;
delete from information_schema.tables where NULL = NULL;
delete from pg_catalog.pg_proc;
delete from pg_catalog.pg_attribute;
delete from pg_catalog.pg_attrdef;
delete from pg_catalog.pg_settings;
delete from pg_catalog.pg_database;
delete from pg_catalog.pg_roles;
delete from pg_catalog.pg_user;

select '-- Test of ERR_TABLE_READONLY error in a table created with READONLY';
create table test1 (id INTEGER PRIMARY KEY) READONLY;
delete from test1;
drop table test1;

select '-- Test same table created with READWRITE works fine with DELETE FROM';
create table test1 (id INTEGER PRIMARY KEY) READWRITE;
delete from test1;
drop table test1;

select '-- Test of ERR_TABLE_READONLY error in a table created with READONLY implicitly assumed';
create table test1 (id INTEGER PRIMARY KEY, firstname VARCHAR DELIM "", lastname VARCHAR DELIM "");
delete from test1;
drop table test1;

