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

create table test (tob time(6));
insert into test values(time'01:01:01.999999');
select * from test;

create table test1 (tsob timestamp(6));
insert into test1 values(timestamp'01-01-2023 01:01:01.999999');
select * from test1;

create table testa (tob time(6) without time zone);
insert into testa values(time'01:01:01.999999');
select * from testa;

create table test1a (tsob timestamp(6) without time zone);
insert into test1a values(timestamp'01-01-2023 01:01:01.999999');
select * from test1a;

create table test2 (tob time(6) with time zone);
insert into test2 values(time with time zone'01:01:01.999999');
select * from test2;

create table test3 (tsob timestamp(6) with time zone);
insert into test3 values(timestamp with time zone'01-01-2023 01:01:01.999999');
select * from test3;

create table test4 (tob time(10));
create table test5 (tsob timestamp(10));

select timestamp'01-01-2023 01:01:01.0001';
select timestamp'01-01-2023 01:01:01.01';
select time'01:01:01.001';
