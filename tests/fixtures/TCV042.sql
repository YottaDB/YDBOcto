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

-- https://gitlab.com/yottadb/dbms/ydbocto/-/merge_requests/1244#note_1388646881
drop view if exists TCV042_1v2156a;
drop view if exists TCV042_1v2156;
create view TCV042_1v2156 as select 1 from (select * from names) n1 group by n1.* order by n1.* is not null;
select * from TCV042_1v2156;
create view TCV042_1v2156a as select * from TCV042_1v2156 union select * from TCV042_1v2156;
select * from TCV042_1v2156a;
drop view if exists TCV042_1v2156a;
drop view if exists TCV042_1v2156;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1388621880
create view TCV042_1v1 as select * from names n1 where exists (select * from names n2) and (n1.id < 3 or n1.id > 3);
select * from TCV042_1v1 n1 where exists (select * from TCV042_1v1 n2) and (n1.id < 3 or n1.id > 3);
drop view if exists TCV042_1v1;

create view TCV042_1v69 as select * from names where exists (select * from names where firstname = 'zero') and (id < 3 or id > 3);
create view TCV042_1v69d as select * from TCV042_1v69 where exists (select * from TCV042_1v69 where firstname = 'zero') and (id < 3 or id > 3);
select count(*) as "count_v69d" from TCV042_1v69d;
drop view if exists TCV042_1v69d;
drop view if exists TCV042_1v69;

create view TCV042_1v346 as select * from names n1 where     exists (select n2.id from names n2 where n2.id = n1.id + 2) or  n1.id > 2;
create view TCV042_1v346d as select * from TCV042_1v346 n1 where     exists (select n2.id from TCV042_1v346 n2 where n2.id = n1.id + 2) or  n1.id > 2;
select count(*) as "count_v346d" from TCV042_1v346d;
drop view if exists TCV042_1v346d;
drop view if exists TCV042_1v346;

create view TCV042_1v392 as select * from names n1 where n1.id > 2  or     exists (select n2.id from names n2 where n2.id = n1.id + 2);
create view TCV042_1v392d as select * from TCV042_1v392 n1 where n1.id > 2  or     exists (select n2.id from TCV042_1v392 n2 where n2.id = n1.id + 2);
select count(*) as "count_v392d" from TCV042_1v392d;
create view TCV042_1v392e as select * from TCV042_1v392d n1 where n1.id > 2  or     exists (select n2.id from TCV042_1v392d n2 where n2.id = n1.id + 2);
select count(*) as "count_v392e" from TCV042_1v392e;
drop view if exists TCV042_1v392e;
drop view if exists TCV042_1v392d;
drop view if exists TCV042_1v392;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1387899721
drop view if exists TCV042_1v6;
drop view if exists TCV042_1v5;
drop view if exists TCV042_1v4;
drop view if exists TCV042_1v3;
drop view if exists TCV042_1v2;
drop view if exists TCV042_1v1;
drop view if exists TCV042_1v;
-- Example from Octo doc which performs dnf boolean expression expansion
create view TCV042_1v as select * from names where lastname = 'Cool' AND (firstname = 'Zero' OR lastname = 'Burn');
create view TCV042_1v1 as select * from TCV042_1v union all select * from TCV042_1v;
create view TCV042_1v2 as select * from TCV042_1v1 union all select * from TCV042_1v1;
select * from TCV042_1v1;
select * from TCV042_1v2;
drop view if exists TCV042_1v2;
drop view if exists TCV042_1v1;
drop view if exists TCV042_1v;

-- Following query is from #814 they test outer query grouped column used in inner query
drop view if exists TCV042_1v2;
drop view if exists TCV042_1v1;
create view TCV042_1v1 as select n1.firstname,count(*) from names n1 group by n1.firstname having exists (select n1.firstname from names n2 group by n2.firstname);
select * from TCV042_1v1;
select n1.firstname,count(*) from TCV042_1v1 n1 group by n1.firstname having exists (select n1.firstname from TCV042_1v1 n2 group by n2.firstname);
drop view if exists TCV042_1v1;

-- Below query is created from a query in https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/759#note_736104049
-- This tests ORDER BY with TABLENAME.ASTERISK where we expect 'Burn' to show up as the first row. Not 'Cool'.
drop view if exists TCV042_1v1;
create view TCV042_1v1 as select t1.id, t1.lastname, t2.* from names t1 left join (select lastname as lstname from names) as t2 on (t1.firstname <= 'Acid') where t1.id <= 1 and t2.lstname is null order by t2.*,t1.id;
select * from TCV042_1v1;
create view TCV042_1v2 as select * from names;
select t1.id, t1.lastname, t2.* from TCV042_1v2 t1 left join (select lastname from TCV042_1v2) as t2 on (t1.firstname <= 'Acid') where t1.id <= 1 and t2.lastname is null order by t2.*,t1.id;
drop view if exists TCV042_1v2;
drop view if exists TCV042_1v1;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1443787266
CREATE VIEW TCV042_1v1681 as SELECT * FROM names WHERE (id = 0 OR id = 5) AND (lastName = 'Cool' OR lastName = 'Cool');
CREATE VIEW TCV042_1v1681a as SELECT * FROM TCV042_1v1681 UNION SELECT * FROM TCV042_1v1681;
select * from TCV042_1v1681a;
drop view if exists TCV042_1v1681a;
drop view if exists TCV042_1v1681;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1446104754
drop view if exists TCV042_1652d;
drop view if exists TCV042_1652;
drop view if exists TCV042_1339;
CREATE VIEW TCV042_1339 as select * from names n1 where (n1.id NOT IN (1,NULL,2));
CREATE VIEW TCV042_1652 as SELECT * FROM names WHERE EXISTS (VALUES ((SELECT id FROM names WHERE id > 7)));
CREATE VIEW TCV042_1652d as SELECT * FROM TCV042_1652 WHERE EXISTS (VALUES ((SELECT id FROM TCV042_1652 WHERE id > 7)));
select * from TCV042_1652d n1 where NOT EXISTS (select n2.id from TCV042_1339 n2 where n2.id = n1.id + 2) OR  n1.id > 2;
drop view if exists TCV042_1652d;
drop view if exists TCV042_1652;
drop view if exists TCV042_1339;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1450115723
CREATE VIEW TCV042_1306 as SELECT * FROM names WHERE NOT FALSE;
CREATE VIEW TCV042_1306d as SELECT * FROM TCV042_1306 WHERE NOT FALSE;
CREATE VIEW TCV042_1306e as SELECT * FROM TCV042_1306d WHERE NOT FALSE;
SELECT * FROM TCV042_1306 WHERE EXISTS (SELECT * FROM TCV042_1306e WHERE firstname = 'Zero') AND (id < 3 OR id > 3);
drop view if exists TCV042_1306e;
drop view if exists TCV042_1306d;
drop view if exists TCV042_1306;
