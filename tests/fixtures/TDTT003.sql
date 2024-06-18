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

-- type:date format:
drop table if exists test;
create table test (id integer, dob date );
\d test;
insert into test values(1,date'2023-01-01');
insert into test values(2,date'2023-01-02');
select * from test;

-- type:date format:fileman
drop table if exists test;
create table test (id integer, dob date(fileman));
\d test;
insert into test values(1,date'2023-01-01');
insert into test values(2,date'2023-01-02');
select * from test;

-- type:date format:horolog
drop table if exists test;
create table test (id integer, dob date(horolog));
\d test;
insert into test values(1,date'2023-01-01');
insert into test values(2,date'2023-01-02');
select * from test;

-- type:date format:zhorolog
drop table if exists test;
create table test (id integer, dob date(zhorolog));
\d test;
insert into test values(1,date'2023-01-01');
insert into test values(2,date'2023-01-02');
select * from test;

-- type:date format:zut
drop table if exists test;
create table test (id integer, dob date(zut));
\d test;
insert into test values(1,date'2023-01-01');
insert into test values(2,date'2023-01-02');
select * from test;


-- type:time format:
drop table if exists test;
create table test (id integer, dob time );
\d test;
insert into test values(1,time'01:01:01');
insert into test values(2,time'01:02:01');
select * from test;

-- type:time format:fileman
drop table if exists test;
create table test (id integer, dob time(fileman));
\d test;
insert into test values(1,time'01:01:01');
insert into test values(2,time'01:02:01');
select * from test;

-- type:time format:horolog
drop table if exists test;
create table test (id integer, dob time(horolog));
\d test;
insert into test values(1,time'01:01:01');
insert into test values(2,time'01:02:01');
select * from test;

-- type:time format:zhorolog
drop table if exists test;
create table test (id integer, dob time(zhorolog));
\d test;
insert into test values(1,time'01:01:01');
insert into test values(2,time'01:02:01');
select * from test;

-- type:time format:zut
drop table if exists test;
create table test (id integer, dob time(zut));
\d test;
insert into test values(1,time'01:01:01');
insert into test values(2,time'01:02:01');
select * from test;

-- type:timestamp format:
drop table if exists test;
create table test (id integer, dob timestamp );
\d test;
insert into test values(1,timestamp'2023-01-01 01:01:01');
insert into test values(2,timestamp'2023-01-02 01:01:01');
select * from test;

-- type:timestamp format:fileman
drop table if exists test;
create table test (id integer, dob timestamp(fileman));
\d test;
insert into test values(1,timestamp'2023-01-01 01:01:01');
insert into test values(2,timestamp'2023-01-02 01:01:01');
select * from test;

-- type:timestamp format:horolog
drop table if exists test;
create table test (id integer, dob timestamp(horolog));
\d test;
insert into test values(1,timestamp'2023-01-01 01:01:01');
insert into test values(2,timestamp'2023-01-02 01:01:01');
select * from test;

-- type:timestamp format:zhorolog
drop table if exists test;
create table test (id integer, dob timestamp(zhorolog));
\d test;
insert into test values(1,timestamp'2023-01-01 01:01:01');
insert into test values(2,timestamp'2023-01-02 01:01:01');
select * from test;

-- type:timestamp format:zut
drop table if exists test;
create table test (id integer, dob timestamp(zut));
\d test;
insert into test values(1,timestamp'2023-01-01 01:01:01');
insert into test values(2,timestamp'2023-01-02 01:01:01');
select * from test;


-- type:timestamp with time zone format:
drop table if exists test;
create table test (id integer, dob timestamp with time zone );
\d test;
insert into test values(1,timestamp with time zone'2023-01-01 01:01:01-05:00');
insert into test values(2,timestamp with time zone'2023-01-02 01:01:01-05:00');
select * from test;

-- type:timestamp with time zone format:fileman
drop table if exists test;
create table test (id integer, dob timestamp(fileman) with time zone);
\d test;
insert into test values(1,timestamp with time zone'2023-01-01 01:01:01-05:00');
insert into test values(2,timestamp with time zone'2023-01-02 01:01:01-05:00');
select * from test;

-- type:timestamp with time zone format:horolog
drop table if exists test;
create table test (id integer, dob timestamp(horolog) with time zone);
\d test;
insert into test values(1,timestamp with time zone'2023-01-01 01:01:01-05:00');
insert into test values(2,timestamp with time zone'2023-01-02 01:01:01-05:00');
select * from test;

-- type:timestamp with time zone format:zhorolog
drop table if exists test;
create table test (id integer, dob timestamp(zhorolog) with time zone);
\d test;
insert into test values(1,timestamp with time zone'2023-01-01 01:01:01-05:00');
insert into test values(2,timestamp with time zone'2023-01-02 01:01:01-05:00');
select * from test;

-- type:timestamp with time zone format:zut
drop table if exists test;
create table test (id integer, dob timestamp(zut) with time zone);
\d test;
insert into test values(1,timestamp with time zone'2023-01-01 01:01:01-05:00');
insert into test values(2,timestamp with time zone'2023-01-02 01:01:01-05:00');
select * from test;


