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

-- View column names with alias section
create view TCV003v1 (TCV003v1_id,TCV003v1_firstname,TCV003v1_lastname) as select id, firstname,lastname from names n1;
select * from TCV003v1;
drop view TCV003v1;

create view TCV003v1 (TCV003v1_id,TCV003v1_lastname) as select id, firstname,lastname from names n1;
select * from TCV003v1;
drop view TCV003v1;

create view TCV003v1 (TCV003v1_id,TCV003v1_lastname) as select id, firstname as n1_firstname,lastname from names n1;
select * from TCV003v1;
drop view TCV003v1;

create view TCV003v1 (TCV003v1_id,TCV003v1_firstname,TCV003v1_lastname) as select id as n1_id, firstname as n1_firstname,lastname as n1_lastname from names n1;
select * from TCV003v1;
drop view TCV003v1;

create view TCV003v1 (TCV003v1_id,TCV003v1_firstname,TCV003v1_lastname) as select * from names n1;
select * from TCV003v1;
drop view TCV003v1;

create view TCV003v1 (TCV003v1_id,TCV003v1_firstname,TCV003v1_lastname) as select n1.* from names n1;
select * from TCV003v1;
drop view TCV003v1;

create view TCV003v1 (TCV003v1_id,TCV003v1_firstname,TCV003v1_lastname) as select names.* from names;
select * from TCV003v1;
drop view TCV003v1;

create view TCV003v1 (TCV003v1_id,TCV003v1_lastname) as select names.* from names;
select * from TCV003v1;
drop view TCV003v1;

-- Values with column reference
create view TCV003v1 (TCV003v1_col1) as select (values(lastname)) from names;
select * from TCV003v1;
drop view TCV003v1;

-- Static values at view definition with aliases
create view TCV003v1 (TCV003v1_col1,TCV003v1_col2,TCV003v1_col3) as select 1,'Zero','Cool';
select * from TCV003v1;
drop view TCV003v1;

create view TCV003v1 (TCV003v1_id,TCV003v1_firstname,TCV003v1_lastname) as select 1 as n1_id,'Zero' as n1_firstname, 'Cool' as n1_firstname;
select * from TCV003v1;
drop view TCV003v1;

-- Static values at view definitino with aliases and values usage
create view TCV003v1 (TCV003v1_col1,TCV003v1_col2,TCV003v1_col3) as values(1,'Zero','Cool');
select * from TCV003v1;
drop view TCV003v1;

create view TCV003v1 (TCV003v1_col1,TCV003v1_col2,TCV003v1_col3) as (values(1,'Zero','Cool'),(2,'Z','C'));
select * from TCV003v1;
drop view TCV003v1;

create view TCV003v1 (TCV003v1_col1,TCV003v1_col2,TCV003v1_col3) as values(1,'Zero','Cool'),(2,'Z','C');
select * from TCV003v1;
drop view TCV003v1;

create view TCV003v1 (TCV003v1_col1) as select (values(1)) from names;
select * from TCV003v1;
drop view TCV003v1;

-- Set operation
create view TCV003v1 (TCV003v1_firstname) as select firstname from names union select lastname from names;
select * from TCV003v1;
drop view TCV003v1;

create view TCV003v1 (TCV003v1_firstname) as select firstname from names union select lastname as test from names;
select * from TCV003v1;
drop view TCV003v1;

create view TCV003v1 (TCV003v1_firstname) as select firstname as test1 from names union select lastname as test from names;
select * from TCV003v1;
drop view TCV003v1;

create view TCV003v1 (TCV003v1_firstname,TCV003v1_lastname) as select firstname,lastname from names union select firstname,lastname from names;
select * from TCV003v1;
drop view TCV003v1;

-- Static values at set operation
create view TCV003v1 (TCV003v1_firstname) as select 'Zero' union select 'Cool';
select * from TCV003v1;
drop view TCV003v1;

create view TCV003v1 (TCV003v1_firstname) as select 'Zero' as test union select 'Cool';
select * from TCV003v1;
drop view TCV003v1;

create view TCV003v1 (TCV003v1_firstname) as select 'Zero' union select 'Cool' as test;
select * from TCV003v1;
drop view TCV003v1;

-- View without view column names section
create view TCV003v1 as select id,firstname,lastname from names n1;
select * from TCV003v1;
drop view TCV003v1;

create view TCV003v1 as select * from names n1;
select * from TCV003v1;
drop view TCV003v1;

create view TCV003v1 as select n1.* from names n1;
select * from TCV003v1;
drop view TCV003v1;

create view TCV003v1 as select id as n1_id, firstname as n1_firstname, lastname as n1_lastname from names n1;
select * from TCV003v1;
drop view TCV003v1;

create view TCV003v1 as select id as n1_id, firstname, lastname as n1_lastname from names n1;
select * from TCV003v1;
drop view TCV003v1;

-- Values with column reference
create view TCV003v1 as select (values(lastname)) from names; -- values
select * from TCV003v1;
drop view TCV003v1;

-- Static values at view definition
create view TCV003v1 as select 1,'Zero' as col2,'Cool' as col3; -- first column is "?column?" --
select * from TCV003v1;
drop view TCV003v1;

create view TCV003v1 as select 1 as col1,'Zero' as col2,'Cool' as col3;
select * from TCV003v1;
drop view TCV003v1;

-- Static values at view definition and values usage
create view TCV003v1 as values(1,'Zero','Cool');
select * from TCV003v1;
drop view TCV003v1;

create view TCV003v1 as (values(1,'Zero','Cool'),(2,'Z','C'));
select * from TCV003v1;
drop view TCV003v1;

create view TCV003v1 as values(1,'Zero','Cool'),(2,'Z','C');
select * from TCV003v1;
drop view TCV003v1;

create view TCV003v1 as select (values(1)) from names;
select * from TCV003v1;
drop view TCV003v1;

-- Set operation
create view TCV003v1 as select firstname from names union select lastname from names;
select * from TCV003v1;
drop view TCV003v1;

create view TCV003v1 as select firstname from names union select lastname as test from names; -- the column name is taken from the first operand of the set operation
select * from TCV003v1;
drop view TCV003v1;

create view TCV003v1 as select firstname as test1 from names union select lastname as test from names; -- the column name is taken from the first operand of the set operation
select * from TCV003v1;
drop view TCV003v1;

create view TCV003v1 as select firstname,lastname from names union select firstname,firstname from names; -- duplicate names in the second operand of set operation doesn't result in an error
select * from TCV003v1;
drop view TCV003v1;

-- Static values at set operation
create view TCV003v1 as select 'Zero' union select 'Cool';
select * from TCV003v1;
drop view TCV003v1;

create view TCV003v1 as select 'Zero' as test union select 'Cool';
select * from TCV003v1;
drop view TCV003v1;

create view TCV003v1 as select 'Zero' union select 'Cool' as test;
select * from TCV003v1;
drop view TCV003v1;

create view TCV003v1 as select 'Zero','Zero1' as col2 union select 'Cool','Cool1';
select * from TCV003v1;
drop view TCV003v1;

