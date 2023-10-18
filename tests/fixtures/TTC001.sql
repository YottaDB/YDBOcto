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

-- TTC001 : OCTO1019 : Type cast literals before storing in INTEGER or NUMERIC column values

---------------------------------------------------------------------------------------------
-- Most of the below queries are from https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/1019#description
-- The remaining queries were added at a later point so are present only below and not in the above url.
--
-- A few queries below are commented below. That is because they generate errors and the error output
-- is not easy to cross check between Octo and Postgres. Those queries are tested separately in
-- the caller script.
---------------------------------------------------------------------------------------------
drop table if exists tblINT;
create table tblINT (id integer primary key, value integer);
insert into tblINT values (1, 2.0);
insert into tblINT values (2, 19);
insert into tblINT values (3, 023);
insert into tblINT values (4, 19.4999);
insert into tblINT values (5, 19.5000);
insert into tblINT values (6, 19.5001);
-- insert into tblINT values (5.499, 7);
-- insert into tblINT values (5.5, 8);
-- insert into tblINT values (5.501, 9);
insert into tblINT values (7.499, 7);
insert into tblINT values (7.5, 8);
insert into tblINT values (8.501, 9);
insert into tblINT values (19, 10);
insert into tblINT values (023, 11);
select * from tblINT;
select * from tblINT where value > 2.0;
select * from tblINT where value > 19.4999;
select * from tblINT where value >= 19.4999;
select * from tblINT where value <= 19.5001;
select * from tblINT where value <= 19.4999;
select * from tblINT where value > 2;
select * from tblINT where value = 2;
select * from tblINT where id > 2.0;
select * from tblINT where id > 5;
select * from tblINT where id > 2;
select * from tblINT where id > 022;
select * from tblINT where id >= 023;
select * from tblINT where id > 022.5;
select * from tblINT where id >= 023.0;
select * from tblINT where value = 2;
select * from tblINT where value = 23.0;
select * from tblINT where value = 023;
select * from tblINT where value = 023.0;
select * from tblINT where value = 023.5;
select * from tblINT where value = 023.49;

drop table if exists tblNUM;
create table tblNUM (id numeric primary key, value numeric);
insert into tblNUM values (1.35, 2.0);
insert into tblNUM values (2.32, 19);
insert into tblNUM values (-3.25, 023);
insert into tblNUM values (4.1, 19.4999);
insert into tblNUM values (05, 19.5000);
insert into tblNUM values (6, 19.5001);
insert into tblNUM values (5.499, 00.17);
insert into tblNUM values (5.5, -00823);
insert into tblNUM values (5.501, 9);
insert into tblNUM values (7.499, 7);
insert into tblNUM values (7.5, 8);
insert into tblNUM values (8.501, 9);
insert into tblNUM values (19, 10);
insert into tblNUM values (023, 11);
select * from tblNUM;
select * from tblNUM where value > 2.0;
select * from tblNUM where value > 19.4999;
select * from tblNUM where value >= 19.4999;
select * from tblNUM where value <= 19.5001;
select * from tblNUM where value <= 19.4999;
select * from tblNUM where value > 2;
select * from tblNUM where value = 2;
select * from tblNUM where id > 2.0;
select * from tblNUM where id > 5;
select * from tblNUM where id > 2;
select * from tblNUM where value = 2;
select * from tblNUM where value = 02.5;
select * from tblNUM where value < 02.5;
select * from tblNUM where value > 02.5;
select * from tblNUM where value > -00824.5;
select * from tblNUM where value <= -001.008;

