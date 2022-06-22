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

-- TC062 : OCTO633 : EXTRACT doesn't parse names of global variables as key or value specifications

create table longkeys (id INTEGER PRIMARY KEY, value VARCHAR) GLOBAL "^longkeys(keys(""ID""))";
select * from longkeys;

create table longvalues (id INTEGER PRIMARY KEY, value VARCHAR) GLOBAL "^longvalues(values(""VALUE""))";
select * from longvalues;

-- The following statements test whether an error is issued for erroneous usages
-- of `keys(..)` expressions. However, these tests are currently disabled since
-- the mechanism for validating these usages is not yet implemented. Rather,
-- when YDBOcto#918 is resolved, then these tests can be re-enabled.

-- Test prohibition of nested `keys(..)` expressions
-- drop table if exists tmp;
-- create table tmp (id integer primary key) GLOBAL "^names(keys(keys(""ID"")))";
-- select * from tmp;

-- Test prohibition of `keys(..)` expressions whose case does not match that of
-- the column name it corresponds to, e.g. uppercase column name, but lowercase
-- name used in `keys(..)` expression.
-- drop table if exists tmp;
-- create table tmp (id integer primary key) GLOBAL "^names(keys(""id""))";
-- select * from tmp;

