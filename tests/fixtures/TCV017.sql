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

-- Joins in view definition
-- join participants are table and view
create view TCV017v1 as select id0,id1,id2,id3,id4,id5,id6,id7,name from composite;
create view TCV017v2 as select TCV017v1.id0 as TCV017v1_0,TCV017v1.id1 as TCV017v1_1,TCV017v1.id2 as TCV017v1_2,TCV017v1.id3 as TCV017v1_3,TCV017v1.id4 as TCV017v1_4,TCV017v1.id5 as TCV017v1_5,TCV017v1.id6 as TCV017v1_6,TCV017v1.id7 as TCV017v1_7,TCV017v1.name as TCV017v1_name,n2.id0 as n2_0,n2.id1 as n2_1,n2.id2 as n2_2,n2.id3 as n2_3,n2.id4 as n2_4,n2.id5 as n2_5,n2.id6 as n2_6,n2.id7 as n2_7,n2.name as n2_name from TCV017v1,composite as n2;
select * from TCV017v2;
select * from TCV017v2,TCV017v2 as n3; -- query with 10000 rows in result, only run it once
drop view TCV017v2;
drop view TCV017v1;

create view TCV017v1 as select id0,id1,id2,id3,id4,id5,id6,id7,name from composite;
create view TCV017v2 as select TCV017v1.id0 as TCV017v1_0,TCV017v1.id1 as TCV017v1_1,TCV017v1.id2 as TCV017v1_2,TCV017v1.id3 as TCV017v1_3,TCV017v1.id4 as TCV017v1_4,TCV017v1.id5 as TCV017v1_5,TCV017v1.id6 as TCV017v1_6,TCV017v1.id7 as TCV017v1_7,TCV017v1.name as TCV017v1_name,n2.id0 as n2_0,n2.id1 as n2_1,n2.id2 as n2_2,n2.id3 as n2_3,n2.id4 as n2_4,n2.id5 as n2_5,n2.id6 as n2_6,n2.id7 as n2_7,n2.name as n2_name from TCV017v1 inner join composite as n2 on TCV017v1.id0 = n2.id0;
create view TCV017v3 as select TCV017v1.id0 as TCV017v1_0,TCV017v1.id1 as TCV017v1_1,TCV017v1.id2 as TCV017v1_2,TCV017v1.id3 as TCV017v1_3,TCV017v1.id4 as TCV017v1_4,TCV017v1.id5 as TCV017v1_5,TCV017v1.id6 as TCV017v1_6,TCV017v1.id7 as TCV017v1_7,TCV017v1.name as TCV017v1_name,n2.id0 as n2_0,n2.id1 as n2_1,n2.id2 as n2_2,n2.id3 as n2_3,n2.id4 as n2_4,n2.id5 as n2_5,n2.id6 as n2_6,n2.id7 as n2_7,n2.name as n2_name from TCV017v1 inner join composite as n2 on TCV017v1.name = n2.name;
select * from TCV017v2;
select * from TCV017v3;
drop view TCV017v2;
drop view TCV017v3;
drop view TCV017v1;

create view TCV017v1 as select id0,id1,id2,id3,id4,id5,id6,id7,name from composite;
create view TCV017v2 as select TCV017v1.id0 as TCV017v1_0,TCV017v1.id1 as TCV017v1_1,TCV017v1.id2 as TCV017v1_2,TCV017v1.id3 as TCV017v1_3,TCV017v1.id4 as TCV017v1_4,TCV017v1.id5 as TCV017v1_5,TCV017v1.id6 as TCV017v1_6,TCV017v1.id7 as TCV017v1_7,TCV017v1.name as TCV017v1_name,n2.id0 as n2_0,n2.id1 as n2_1,n2.id2 as n2_2,n2.id3 as n2_3,n2.id4 as n2_4,n2.id5 as n2_5,n2.id6 as n2_6,n2.id7 as n2_7,n2.name as n2_name from TCV017v1 left join composite as n2 on TCV017v1.id0 = n2.id0;
create view TCV017v3 as select TCV017v1.id0 as TCV017v1_0,TCV017v1.id1 as TCV017v1_1,TCV017v1.id2 as TCV017v1_2,TCV017v1.id3 as TCV017v1_3,TCV017v1.id4 as TCV017v1_4,TCV017v1.id5 as TCV017v1_5,TCV017v1.id6 as TCV017v1_6,TCV017v1.id7 as TCV017v1_7,TCV017v1.name as TCV017v1_name,n2.id0 as n2_0,n2.id1 as n2_1,n2.id2 as n2_2,n2.id3 as n2_3,n2.id4 as n2_4,n2.id5 as n2_5,n2.id6 as n2_6,n2.id7 as n2_7,n2.name as n2_name from TCV017v1 left join composite as n2 on TCV017v1.name = n2.name;
select * from TCV017v2;
select * from TCV017v3;
drop view TCV017v2;
drop view TCV017v3;
drop view TCV017v1;


create view TCV017v1 as select id0,id1,id2,id3,id4,id5,id6,id7,name from composite;
create view TCV017v2 as select TCV017v1.id0 as TCV017v1_0,TCV017v1.id1 as TCV017v1_1,TCV017v1.id2 as TCV017v1_2,TCV017v1.id3 as TCV017v1_3,TCV017v1.id4 as TCV017v1_4,TCV017v1.id5 as TCV017v1_5,TCV017v1.id6 as TCV017v1_6,TCV017v1.id7 as TCV017v1_7,TCV017v1.name as TCV017v1_name,n2.id0 as n2_0,n2.id1 as n2_1,n2.id2 as n2_2,n2.id3 as n2_3,n2.id4 as n2_4,n2.id5 as n2_5,n2.id6 as n2_6,n2.id7 as n2_7,n2.name as n2_name from TCV017v1 right join composite as n2 on TCV017v1.id0 = n2.id0;
create view TCV017v3 as select TCV017v1.id0 as TCV017v1_0,TCV017v1.id1 as TCV017v1_1,TCV017v1.id2 as TCV017v1_2,TCV017v1.id3 as TCV017v1_3,TCV017v1.id4 as TCV017v1_4,TCV017v1.id5 as TCV017v1_5,TCV017v1.id6 as TCV017v1_6,TCV017v1.id7 as TCV017v1_7,TCV017v1.name as TCV017v1_name,n2.id0 as n2_0,n2.id1 as n2_1,n2.id2 as n2_2,n2.id3 as n2_3,n2.id4 as n2_4,n2.id5 as n2_5,n2.id6 as n2_6,n2.id7 as n2_7,n2.name as n2_name from TCV017v1 right join composite as n2 on TCV017v1.name = n2.name;
select * from TCV017v2;
select * from TCV017v3;
drop view TCV017v2;
drop view TCV017v3;
drop view TCV017v1;

create view TCV017v1 as select id0,id1,id2,id3,id4,id5,id6,id7,name from composite;
create view TCV017v2 as select TCV017v1.id0 as TCV017v1_0,TCV017v1.id1 as TCV017v1_1,TCV017v1.id2 as TCV017v1_2,TCV017v1.id3 as TCV017v1_3,TCV017v1.id4 as TCV017v1_4,TCV017v1.id5 as TCV017v1_5,TCV017v1.id6 as TCV017v1_6,TCV017v1.id7 as TCV017v1_7,TCV017v1.name as TCV017v1_name,n2.id0 as n2_0,n2.id1 as n2_1,n2.id2 as n2_2,n2.id3 as n2_3,n2.id4 as n2_4,n2.id5 as n2_5,n2.id6 as n2_6,n2.id7 as n2_7,n2.name as n2_name from TCV017v1 full outer join composite as n2 on TCV017v1.id0 = n2.id0;
create view TCV017v3 as select TCV017v1.id0 as TCV017v1_0,TCV017v1.id1 as TCV017v1_1,TCV017v1.id2 as TCV017v1_2,TCV017v1.id3 as TCV017v1_3,TCV017v1.id4 as TCV017v1_4,TCV017v1.id5 as TCV017v1_5,TCV017v1.id6 as TCV017v1_6,TCV017v1.id7 as TCV017v1_7,TCV017v1.name as TCV017v1_name,n2.id0 as n2_0,n2.id1 as n2_1,n2.id2 as n2_2,n2.id3 as n2_3,n2.id4 as n2_4,n2.id5 as n2_5,n2.id6 as n2_6,n2.id7 as n2_7,n2.name as n2_name from TCV017v1 full outer join composite as n2 on TCV017v1.name = n2.name;
select * from TCV017v2;
select * from TCV017v3;
drop view TCV017v2;
drop view TCV017v3;
drop view TCV017v1;


create view TCV017v1 as select id0,id1,id2,id3,id4,id5,id6,id7,name from composite;
create view TCV017v2 as select TCV017v1.id0 as TCV017v1_0,TCV017v1.id1 as TCV017v1_1,TCV017v1.id2 as TCV017v1_2,TCV017v1.id3 as TCV017v1_3,TCV017v1.id4 as TCV017v1_4,TCV017v1.id5 as TCV017v1_5,TCV017v1.id6 as TCV017v1_6,TCV017v1.id7 as TCV017v1_7,TCV017v1.name as TCV017v1_name,n2.id0 as n2_0,n2.id1 as n2_1,n2.id2 as n2_2,n2.id3 as n2_3,n2.id4 as n2_4,n2.id5 as n2_5,n2.id6 as n2_6,n2.id7 as n2_7,n2.name as n2_name from TCV017v1 natural join composite as n2;
create view TCV017v3 as select TCV017v1.id0 as TCV017v1_0,TCV017v1.id1 as TCV017v1_1,TCV017v1.id2 as TCV017v1_2,TCV017v1.id3 as TCV017v1_3,TCV017v1.id4 as TCV017v1_4,TCV017v1.id5 as TCV017v1_5,TCV017v1.id6 as TCV017v1_6,TCV017v1.id7 as TCV017v1_7,TCV017v1.name as TCV017v1_name,n2.id0 as n2_0,n2.id1 as n2_1,n2.id2 as n2_2,n2.id3 as n2_3,n2.id4 as n2_4,n2.id5 as n2_5,n2.id6 as n2_6,n2.id7 as n2_7,n2.name as n2_name from TCV017v1 natural join composite n2;
select * from TCV017v2;
select * from TCV017v3;
drop view TCV017v3;
drop view TCV017v2;
drop view TCV017v1;
