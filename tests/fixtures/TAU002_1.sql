#################################################################
#								#
# Copyright (c) 2023-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

create view v1 as select * from names;
select * from v1;

create function absf(integer) returns integer as $$ABS^%ydboctosqlfunctions;
create view v2 as select absf(id) from names;
select * from v2;

create view k1 as select 1;
select * from k1;

create view k2 as select 2;
select * from k2;

-- Views which depend on other Views
create view b1 as select * from names;
select * from b1;

create view a1 as select * from b1;
select * from a1;

CREATE FUNCTION SAMEVALUE(INTEGER) RETURNS INTEGER AS $$samevalue^functions;
create view f1 as select samevalue(1);
select * from f1;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1375758802
create view a as select 1;
create view b as select * from a;
select * from b;
select * from a;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1375502307
create view v9 as select * from octoonerowtable;
create view v10 as select * from v9;
create view v11 as select * from v9;
select * from v9;
select * from v10;
select * from v11;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1375783812
create view v33 as select * from (select * from names) order by lastname;
create view v34 as select * from (select lastname from names union select firstname from names) order by lastname;
create view v35 as select * from (values(1)) order by 1;
select * from v33;
select * from v34;
select * from v35;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379082949
create view v36 as select 1 from (select 1 union select 1);
select * from v36;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379091825
create view v37 as select CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end;
select * from v37;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379101951
create view v38 as select 1 IN (2,3);
select * from v38;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379107070
create view v39 as select sum(mybool::integer) from (select id=2 as mybool from names) n1;
select * from v39;
create view v40 as select count(*) from (select id=2 from names) n1;
select * from v40;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380391980
create view v41 as select id from names n1 where (select n1.id from names n2 where n2.id > 5) is not null;
select * from v41;
