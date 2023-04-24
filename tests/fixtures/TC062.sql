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

-- Test of ERR_VALUES_NOT_ALLOWED_IN_GLOBAL error
create table longvalues (id INTEGER PRIMARY KEY, value VARCHAR) GLOBAL "^longvalues(values(""VALUE""))";

-- Test of ERR_UNKNOWN_COLUMN_NAME error
-- Test prohibition of nested `keys(..)` expressions
create table tmp (id integer primary key) GLOBAL "^names(keys(keys(""ID"")))";

-- Test of ERR_UNKNOWN_COLUMN_NAME error
-- Test prohibition of `keys(..)` expressions whose case does not match that of the column name it corresponds to,
-- e.g. uppercase column name, but lowercase name used in `keys(..)` expression.
create table tmp (id integer primary key) GLOBAL "^names(keys(""id""))";

