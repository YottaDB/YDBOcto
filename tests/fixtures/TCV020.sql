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

-- Expect the following error select on the following view definitions
-- [ERROR]: ERR_SUBQUERY_MULTIPLE_ROWS: More than one row returned by a subquery used as an expression
create view v1 (v1_col1) as select (values(lastname),(firstname)) from names;
select * from v1;
drop view v1;

create view v1 as select (values(lastname),(firstname)) from names;
select * from v1;
drop view v1;

create view v1 as select (select 1 union select 2) from names;
select * from v1;
drop view v1;

create view v1 as select (select * from names) from names;
drop view v1;

-- Expect the following error for the following view definition
-- [ERROR]: ERR_SUBQUERY_ONE_COLUMN: Subquery must return only one column
create view v1 as select (select 1,2) from names;
create view v1 as select (values(1,2)) from names;
create view v1 as select (select 1,2 union select 2,3) from names;

