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

drop view if exists TCV033_1namesv1;
create view TCV033_1namesv1 as select * from names;
select (select * from (select n3.id from TCV033_1namesv1 n1) n2 limit 1) from TCV033_1namesv1 n3;
-- Following query is commented as Postgres generates an error for it. https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/962 tracks the issue.
-- select * from TCV033_1namesv1 n3 full join TCV033_1namesv1 n4 on exists (select * from (select n3.* from TCV033_1namesv1 n1)n2);
select (select * from (select * from (select * from (select distinct n1.id from TCV033_1namesv1 n1 where n1.id = n5.id) n2) n3) n4) from TCV033_1namesv1 n5;
select (select * from (select * from (select n1.id from TCV033_1namesv1 n1 where n1.id = n4.id) n2) n3) from TCV033_1namesv1 n4;
select (select * from (select n1.id from TCV033_1namesv1 n1 where n1.id = n3.id) n2) from TCV033_1namesv1 n3;
select (select * from (select n1.id from TCV033_1namesv1 n1 where n1.id = n4.id union select n2.id from TCV033_1namesv1 n2 where n2.id = n4.id) n3) from TCV033_1namesv1 n4;
select (select * from (select n2.id from (select n1.id from TCV033_1namesv1 n1 where n1.id = n4.id) n2) n3) from TCV033_1namesv1 n4;
select (select * from (select n3.id from TCV033_1namesv1 n4 order by n3.id limit 1) n2) from TCV033_1namesv1 n3;
select (select firstname from (select n3.id,firstname from TCV033_1namesv1 n1) n2 order by 1 desc limit 1) from TCV033_1namesv1 n3;
select (select firstname from (select n3.id,firstname from TCV033_1namesv1 n1) n2 limit 1) from TCV033_1namesv1 n3;
select (select n2.id from (select n1.id from TCV033_1namesv1 n1 where n1.id = n3.id) n2) from TCV033_1namesv1 n3;
select (select n2.id from (select n3.id from TCV033_1namesv1 n4) n2 inner join TCV033_1namesv1 n1 on n1.id = n3.id + 1 limit 1) from TCV033_1namesv1 n3;
select (select n3.id from (select * from (select n1.id from TCV033_1namesv1 n1 where n1.id = n4.id) n2) n3) from TCV033_1namesv1 n4;
select (select n3.id from (select n2.id from (select n1.id from TCV033_1namesv1 n1 where n1.id = n4.id) n2) n3) from TCV033_1namesv1 n4;
select (select n3.id from (select n3.id from TCV033_1namesv1 n4 order by n3.id limit 1) n2 inner join TCV033_1namesv1 n1 on n1.id = n3.id) from TCV033_1namesv1 n3;
select * from TCV033_1namesv1 where (id = (select x from (values(id)) as tbl(x)));
select * from TCV033_1namesv1 n3 inner join TCV033_1namesv1 n4 on exists (select * from (select n3.* from TCV033_1namesv1 n1) n2);
select * from TCV033_1namesv1 n3 left join TCV033_1namesv1 n4 on exists (select * from (select n3.* from TCV033_1namesv1 n1) n2);
select * from TCV033_1namesv1 n3 right join TCV033_1namesv1 n4 on exists (select * from (select n3.* from TCV033_1namesv1 n1) n2);
select * from TCV033_1namesv1 n3 where (1 = (select id from (select * from (select n3.* from TCV033_1namesv1 n1) n4) n2 limit 1));
select * from TCV033_1namesv1 n3 where exists (select * from (select n3.* from TCV033_1namesv1 n1) n2);
select count(*) from TCV033_1namesv1 n4 group by firstname having exists (select * from TCV033_1namesv1 n3 where exists (select * from (select n3.* from TCV033_1namesv1 n1) n2));
select count(*) from TCV033_1namesv1 n4 group by n4.* having exists (select * from TCV033_1namesv1 n3 where exists (select * from (select n3.* from TCV033_1namesv1 n1) n2));
drop view if exists TCV033_1namesv1;
