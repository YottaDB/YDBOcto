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

-- TCV025 : Sliding window TCV025_1test which adds rows that appear and then after additional updates dissapear from the views range of visibility
-- First TCV025_1test of sliding window
-- The rows seen through the view are only those in range of the view. Rows inserted which doesn't belong to the view's visiblity
-- range are not seen by the select on the view.
drop view if exists TCV025_1v1;
drop table if exists TCV025_1test;
create table TCV025_1test (id integer, name text);
create view TCV025_1v1 as select id,name from TCV025_1test where id >= 2 AND id <= 4;
select * from TCV025_1v1; -- 0 rows visible

insert into TCV025_1test values(1, 'first');
select * from TCV025_1v1; -- 0 rows visible
select * from TCV025_1test; -- 1 row present

insert into TCV025_1test values(2,'second');
select * from TCV025_1v1; -- 1 row is visible
select * from TCV025_1test; -- 2 rows are present

insert into TCV025_1test values(3,'third');
select * from TCV025_1v1; -- 2 rows are visible
select * from TCV025_1test; -- 3 rows are present

insert into TCV025_1test values(4,'fourth');
select * from TCV025_1v1; -- 3 rows are visible
select * from TCV025_1test; -- 4 rows are present

insert into TCV025_1test values(5,'fifth');
select * from TCV025_1v1; -- 3 rows are visible
select * from TCV025_1test; -- 5 rows are present

-- Second TCV025_1test of sliding window
-- Increasing `id` results in rows entering and exiting views range of visiblity
drop view if exists TCV025_1v2;
create view TCV025_1v2 as select id, name from TCV025_1test where id > 6 AND id <= 8;
select * from TCV025_1v2; -- 0 rows visible but TCV025_1test has 5 rows

update TCV025_1test set id=id+1;
select * from TCV025_1v2; -- 0 rows are visible

update TCV025_1test set id=id+1;
select * from TCV025_1v2; -- 1 row is visible (7,'fifth)

update TCV025_1test set id=id+1;
select * from TCV025_1v2; -- 2 rows are visible (8,'fifth') (7,'fourth')

update TCV025_1test set id=id+1;
select * from TCV025_1v2; -- 2 rows are visible (8,'fourth') (7,'third')

update TCV025_1test set id=id+1;
select * from TCV025_1v2; -- 2 rows are visible (8,'third') (7,'second')

update TCV025_1test set id=id+1;
select * from TCV025_1v2; -- 2 rows are visible (8,'second') (7,'first')

update TCV025_1test set id=id+1;
select * from TCV025_1v2; -- 2 rows are visible (8,'first')

update TCV025_1test set id=id+1;
select * from TCV025_1v2; -- 0 rows are visible

update TCV025_1test set id=id+1;
select * from TCV025_1v2; -- 0 rows are visible

update TCV025_1test set id=id+1;
select * from TCV025_1v2; -- 0 rows are visible

drop view TCV025_1v1;
drop view TCV025_1v2;
drop table TCV025_1test;
