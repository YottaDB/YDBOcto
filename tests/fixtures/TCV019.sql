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

-- Set operation
create view TCV019v1 (TCV019v1_firstname) as select firstname from names union select lastname from names;
select * from TCV019v1;
drop view TCV019v1;

create view TCV019v1 (TCV019v1_firstname) as select firstname from names union select lastname as test from names;
select * from TCV019v1;
drop view TCV019v1;

create view TCV019v1 (TCV019v1_firstname) as select firstname as test1 from names union select lastname as test from names;
select * from TCV019v1;
drop view TCV019v1;

create view TCV019v1 (TCV019v1_firstname,TCV019v1_lastname) as select firstname,lastname from names union select firstname,lastname from names;
select * from TCV019v1;
drop view TCV019v1;
-- Static values at set operation
create view TCV019v1 (TCV019v1_firstname) as select 'Zero' union select 'Cool';
select * from TCV019v1;
drop view TCV019v1;

create view TCV019v1 (TCV019v1_firstname) as select 'Zero' as test union select 'Cool';
select * from TCV019v1;
drop view TCV019v1;

create view TCV019v1 (TCV019v1_firstname) as select 'Zero' union select 'Cool' as test;
select * from TCV019v1;
drop view TCV019v1;

-- Set operation
create view TCV019v1 as select firstname from names union select lastname from names;
select * from TCV019v1;
drop view TCV019v1;

create view TCV019v1 as select firstname from names union select lastname as test from names; -- the column name is taken from the first operand of the set operation
select * from TCV019v1;
drop view TCV019v1;

create view TCV019v1 as select firstname as test1 from names union select lastname as test from names; -- the column name is taken from the first operand of the set operation
select * from TCV019v1;
drop view TCV019v1;

create view TCV019v1 as select firstname,lastname from names union select firstname,firstname from names; -- duplicate names in the second operand of set operation doesn't result in an error
select * from TCV019v1;
drop view TCV019v1;

  -- Static values at set operation
create view TCV019v1 as select 'Zero' union select 'Cool';
select * from TCV019v1;
drop view TCV019v1;

create view TCV019v1 as select 'Zero' as test union select 'Cool';
select * from TCV019v1;
drop view TCV019v1;

create view TCV019v1 as select 'Zero' union select 'Cool' as test;
select * from TCV019v1;
drop view TCV019v1;

create view TCV019v1 as select 'Zero','Zero1' as col2 union select 'Cool','Cool1';
select * from TCV019v1;
drop view TCV019v1;

-- VALUES
create view TCV019v1 (TCV019v1_col1) as select (values(lastname)) from names;
select * from TCV019v1;
drop view TCV019v1;

create view TCV019v1 (TCV019v1_col1) as select (values(1)) from names;
select * from TCV019v1;
drop view TCV019v1;

create view TCV019v1 as select (values(lastname)) from names; -- values
select * from TCV019v1;
drop view TCV019v1;

create view TCV019v1 (TCV019v1_col1,TCV019v1_col2,TCV019v1_col3) as values(1,'Zero','Cool');
select * from TCV019v1;
drop view TCV019v1;

create view TCV019v1 (TCV019v1_col1,TCV019v1_col2,TCV019v1_col3) as (values(1,'Zero','Cool'),(2,'Z','C'));
select * from TCV019v1;
drop view TCV019v1;

create view TCV019v1 (TCV019v1_col1,TCV019v1_col2,TCV019v1_col3) as values(1,'Zero','Cool'),(2,'Z','C');
select * from TCV019v1;
drop view TCV019v1;

create view TCV019v1 as values(1,'Zero','Cool');
select * from TCV019v1;
drop view TCV019v1;

create view TCV019v1 as (values(1,'Zero','Cool'),(2,'Z','C'));
select * from TCV019v1;
drop view TCV019v1;

create view TCV019v1 as values(1,'Zero','Cool'),(2,'Z','C');
select * from TCV019v1;
drop view TCV019v1;

create view TCV019v1 as select (values(1)) from names;
select * from TCV019v1;
drop view TCV019v1;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1375820974
create view TCV019v1 as SELECT * FROM ((SELECT * FROM names) UNION (SELECT NULL AS id, NULL AS firstname, NULL AS lastname)) n1 WHERE (id = 0) OR NOT (id = 0) OR (id IS NULL) ORDER BY n1.id;
create view TCV019v2 as SELECT * FROM ((SELECT * FROM names) UNION (SELECT NULL AS id, NULL AS firstname, NULL AS lastname)union (select * from names)) n1 WHERE (id = 0) OR NOT (id = 0) OR (id IS NULL) ORDER BY n1.id;
select * from TCV019v1;
select * from TCV019v2;
drop view TCV019v1;
drop view TCV019v2;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1375825448
create view TCV019v1 as (select * from names UNION select * from names) UNION ALL (select * from names UNION select * from names);
select * from TCV019v1;
drop view TCV019v1;

