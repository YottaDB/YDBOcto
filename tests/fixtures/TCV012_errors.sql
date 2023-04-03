#################################################################
#                                                               #
# Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.       #
# All rights reserved.                                          #
#                                                               #
#       This source code contains the intellectual property     #
#       of its copyright holder(s), and is made available       #
#       under a license.  If you do not know the terms of       #
#       the license, please stop and do not read further.       #
#                                                               #
#################################################################

-- Create view
create view v1 as select 1;
-- Drop view
drop view v1;
-- Attempt to select on v1 should fail
select * from v1;

-- Attempt to drop a view which doesn't exist
drop view v1;
drop view if exists v1;

-- Create views
create view v1 as select 1;
create view v2 as select 1;
-- Attempt to select from the view to check that it is actually created
select * from v1;
select * from v2;
-- Test that drop view on both can be done using a single statement
drop view v1, v2;
-- Attempt to select on both should fail
select * from v1;
select * from v2;

-- Edge case with drop view
-- Create view
create view v1 as select 1;
-- Attempt to select from the view to check that it is actually created
select * from v1;
-- Following drop is intensionally attempted with a non-existant view along with an existant view.
-- We want to verify that if drop of the non-existant view results in an error then the existant view
--   is also not dropped i.e. if there is an error with the drop statement it doesn't go through for any its parameters.
drop view v1,v2;
-- Above statement should have an error as shown below
-- ERROR:  view "v2" does not exist
-- The below select will succeed as v1 is not dropped by the previous statement.
select * from v1;
-- Verify that the same is true irrespective of the position in drop view list
drop view v2,v1;
-- Above statement should have an error as shown below
-- ERROR:  view "v2" does not exist
-- The below select will succeed as v1 is not dropped by the previous statement.
select * from v1;

drop view v1;
drop view v2;
-- Drop View on a table
create table test (id integer);
drop view test;
drop view if exists test;
drop table test;
-- Drop Table on a view
create view v1 as select 1;
drop table v1;
drop table if exists v1;
