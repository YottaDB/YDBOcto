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

create table test (dob date);
insert into test values(date'01-01-2023');
insert into test values(date'01-02-2023');
insert into test values(date'01-02-2023');
insert into test values(date'01-03-2023');
select * from test;
select count(test.*) from test;
select count(distinct test.*) from test;

create table test1 (id int, dob date);
insert into test1 values(1, date'01-01-2023');
insert into test1 values(2, date'01-02-2023');
insert into test1 values(2, date'01-02-2023');
insert into test1 values(3, date'01-03-2023');
select * from test1;
select count(test1.*) from test1;
select count(distinct test1.*) from test1;

create table test2 (tob time);
insert into test2 values(time'01:01:01');
insert into test2 values(time'01:02:01');
insert into test2 values(time'01:02:01');
insert into test2 values(time'01:03:01');
select * from test2;
select count(test2.*) from test2;
select count(distinct test2.*) from test2;
