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

select * from v1; -- View created in TAU002_1.sql exists and it refers to names table
drop table names; -- should result in ERR_DROP_TABLE_DEPENDS_ON_VIEW

select * from v2; -- View created in TAU002_1.sql exists and it uses abs(integer)
drop function abs(integer); -- ERR_DROP_FUNCTION_DEPENDS_ON_VIEW

select * from k1;
select * from k2;
drop table OCTOONEROWTABLE;

drop view b1;
select * from a1;
select * from b1;

drop function samevalue(integer);
select * from f1;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1375758802
drop view a;
select * from b;
select * from a;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1375502307
drop view v9;
select * from v10;
drop view v10;
drop view v9;
select * from v11;
drop view v11;
select * from v9;
drop view v9;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1375783812
drop table names;
select * from v35;
select * from v34;
select * from v33;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379082949
select * from v36;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379091825
select * from v37;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379101951
select * from v38;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379107070
select * from v39;
select * from v40;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380391980
select * from v41;
