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

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1369116065
drop view if exists TCV036_1v2;
drop view if exists TCV036_1v1;
drop view if exists TCV036_1v;
create view TCV036_1v as select * from names;
create view TCV036_1v1 as select * from TCV036_1v;
create view TCV036_1v2 as select TCV036_1v1.id, TCV036_1v1.firstname, TCV036_1v1.lastname from names, TCV036_1v inner join TCV036_1v1 on TCV036_1v1.id = TCV036_1v.id;
select * from TCV036_1v2 limit 1;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1369113098
drop view if exists TCV036_1v2;
drop view if exists TCV036_1v1;
drop view if exists TCV036_1v;
create view TCV036_1v as select * from names;
create view TCV036_1v1 as select * from TCV036_1v;
create view TCV036_1v2 as select TCV036_1v1.id, TCV036_1v1.firstname, TCV036_1v1.lastname from names, TCV036_1v, TCV036_1v1;
select * from TCV036_1v2 limit 1;
drop view if exists TCV036_1v2;
drop view if exists TCV036_1v1;
drop view if exists TCV036_1v;
