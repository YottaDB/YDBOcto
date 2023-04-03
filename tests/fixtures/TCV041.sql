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

-- Column level constraints
drop view if exists TCV041_1v1;
drop table if exists TCV041_1tmp1;
create table TCV041_1tmp1 (id integer unique, name text, value numeric check (value > 0));
insert into TCV041_1tmp1 values(1,'first',1.2);
insert into TCV041_1tmp1 values(2,'second',2.2);
insert into TCV041_1tmp1 values(3,'third',0); -- Fails due to value being less than 0
insert into TCV041_1tmp1 values(2,'third',3.2); -- Fails due to id being not unique
create view TCV041_1v1 as select * from TCV041_1tmp1;
select * from TCV041_1v1;
drop view if exists TCV041_1v1;
drop table if exists TCV041_1tmp1;

-- test column level check constraint with a name is accepted
drop view if exists TCV041_1v2;
drop table if exists TCV041_1tmp2;
create table TCV041_1tmp2 (id integer, name text, value numeric constraint name1 check (value > 0));
insert into TCV041_1tmp2 values(1,'first',1.2);
insert into TCV041_1tmp2 values(2,'second',2.2);
insert into TCV041_1tmp2 values(3,'third',0); -- Fails due to value being less than 0
create view TCV041_1v2 as select * from TCV041_1tmp2;
select * from TCV041_1v2;
drop view if exists TCV041_1v2;
drop table if exists TCV041_1tmp2;

-- table level constraints
-- test table level check constraint without a name is accepted
drop view if exists TCV041_1v3;
drop table if exists TCV041_1tmp3;
create table TCV041_1tmp3 (id integer, check (id > 0),unique(id));
insert into TCV041_1tmp3 values(1);
insert into TCV041_1tmp3 values(2);
insert into TCV041_1tmp3 values(0); -- Fails due to id being less than 0
insert into TCV041_1tmp3 values(1); -- Fails due to id being not unique
create view TCV041_1v3 as select * from TCV041_1tmp3;
select * from TCV041_1v3;
drop view if exists TCV041_1v3;
drop table if exists TCV041_1tmp3;

-- test table level check constraint with a name is accepted
drop view if exists TCV041_1v4;
drop table if exists TCV041_1tmp4;
create table TCV041_1tmp4 (id integer, constraint name1 check (id > 0));
insert into TCV041_1tmp4 values(1);
insert into TCV041_1tmp4 values(2);
insert into TCV041_1tmp4 values(0); -- Fails due to id being less than 0
create view TCV041_1v4 as select * from TCV041_1tmp4;
select * from TCV041_1v4;
drop view if exists TCV041_1v4;
drop table if exists TCV041_1tmp4;
