#################################################################
#								#
# Copyright (c) 2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

drop table if exists test;
create table test (foo timestamp primary key);
insert into test values (timestamp'2023-01-01T01:01:01');
insert into test values (timestamp'2023-01-01 02:01:01');
select * from test order by foo; -- Used to cause an assert fail

drop table if exists test;
create table test (foo date primary key);
insert into test values (date'2023-01-01');
insert into test values (date'2023-01-02');
select * from test order by foo; -- Used to cause an assert fail

drop table if exists test;
create table test (foo timestamp with time zone primary key);
insert into test values (timestamp with time zone'2023-01-01T01:01:01-05');
insert into test values (timestamp with time zone'2023-01-01 02:01:01-05');
select * from test order by foo; -- Used to cause an assert fail

drop table if exists test;
create table test (foo time primary key);
insert into test values (time 'T01:01:01');
insert into test values (time '02:01:01');
select * from test order by foo; -- Used to cause an assert fail

drop table if exists test;
create table test(foo timestamp primary key) global "^testts" readonly;
select * from test order by foo; -- Used to cause an assert fail

drop table if exists test;
create table test(foo Date primary key) global "^testd" readonly;
select * from test order by foo; -- Used to cause an assert fail

drop table if exists test;
create table test(foo Time primary key) global "^testt" readonly;
select * from test order by foo; -- Used to cause an assert fail

drop table if exists test;
create table test(foo Timestamp With Time Zone primary key) global "^testtstz" readonly;
select * from test order by foo; -- Used to cause an assert fail
