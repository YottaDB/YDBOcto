#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TSC21 : Test SELECT COLUMN list values of length ~ 16Kb

create table longvalues (id INTEGER PRIMARY KEY, value VARCHAR) GLOBAL "^longvalues(keys(""id""))";

-- Test queries with huge column values
select id from longvalues order by id;
select id from longvalues where value ~ id::varchar order by id;
select value from longvalues order by id;
select id,value from longvalues order by id;
select l1.id,l2.id from longvalues l1 inner join longvalues l2 ON l1.id = l2.id;

-- Test same queries as above but involving sub-queries with huge column values.
-- The below queries should have the exact same output as the above. This tests OCTO473.
select * from (select id from longvalues) order by id;
select * from (select id from longvalues where value ~ id::varchar) order by id;
select * from (select value from longvalues order by id);
select * from (select id,value from longvalues) order by id;

-- This tests OCTO474.
select l1.id,l2.id from longvalues l1 inner join (select value,id from longvalues) as l2 ON l1.id = l2.id;

