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

set datestyle='MDY';
create table testrw (id int,dob date);
insert into testrw values(1,date'01-01-2023');
insert into testrw values(1,date(fileman)'3230201');
insert into testrw values(2,date'03-01-2023');
select * from testrw;
select * from testrw where dob between date'01-01-2023' AND date'02-01-2023';
select * from testrw where dob > date'01-01-2023';

set datestyle='YMD';
drop table if exists txt;
create table txt (id integer primary key, firstname varchar, lastname varchar, dateofbirth date) GLOBAL "^text" READWRITE;
insert into txt values (1, 'a8', 'b8', '1729-11-24');
insert into txt values (2, 'a9', 'b9', '1774-05-28');
insert into txt values (3, 'a5', 'b5', '2195-08-16');
insert into txt values (4, 'a6', 'b6', '2299-06-28');
insert into txt values (5, 'a7', 'b7', '2418-03-09');
drop table if exists flmn;
create table flmn (id integer primary key, firstname varchar, lastname varchar, dateofbirth date(fileman)) GLOBAL "^fileman" READWRITE;
insert into flmn values (1, 'a5', 'b5', '9830811');
insert into flmn values (2, 'a6', 'b6', '0740528');
insert into flmn values (3, 'a7', 'b7', '4950816');
insert into flmn values (4, 'a8', 'b8', '0291124');
insert into flmn values (5, 'a9', 'b9', '7180309');
select t.id as t_id, t.dateofbirth as t_dateofbirth from txt t order by dateofbirth;
select f.id as f_id, f.dateofbirth as f_dateofbirth from flmn f order by dateofbirth;
select t.id as t_id,f.id as f_id,t.dateofbirth,f.dateofbirth from txt t, flmn f where t.dateofbirth = f.dateofbirth;
