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
drop table if exists test keepdata;
create table test (id integer primary key, dob date ) global "^datetimedate" readonly;
\d test;
select * from test;

-- type:date format:fileman
drop table if exists test keepdata;
create table test (id integer primary key, dob date(fileman)) global "^datetimedatefileman" readonly;
\d test;
select * from test;

-- type:date format:horolog
drop table if exists test keepdata;
create table test (id integer primary key, dob date(horolog)) global "^datetimedatehorolog" readonly;
\d test;
select * from test;

-- type:date format:zhorolog
drop table if exists test keepdata;
create table test (id integer primary key, dob date(zhorolog)) global "^datetimedatezhorolog" readonly;
\d test;
select * from test;

-- type:date format:zut
drop table if exists test keepdata;
create table test (id integer primary key, dob date(zut)) global "^datetimedatezut" readonly;
\d test;
select * from test;


-- type:time format:
drop table if exists test keepdata;
create table test (id integer primary key, dob time ) global "^datetimetime" readonly;
\d test;
select * from test;

-- type:time format:fileman
drop table if exists test keepdata;
create table test (id integer primary key, dob time(fileman)) global "^datetimetimefileman" readonly;
\d test;
select * from test;

-- type:time format:horolog
drop table if exists test keepdata;
create table test (id integer primary key, dob time(horolog)) global "^datetimetimehorolog" readonly;
\d test;
select * from test;

-- type:time format:zhorolog
drop table if exists test keepdata;
create table test (id integer primary key, dob time(zhorolog)) global "^datetimetimezhorolog" readonly;
\d test;
select * from test;

-- type:time format:zut
drop table if exists test keepdata;
create table test (id integer primary key, dob time(zut)) global "^datetimetimezut" readonly;
\d test;
select * from test;

-- type:timestamp format:
drop table if exists test keepdata;
create table test (id integer primary key, dob timestamp ) global "^datetimetimestamp" readonly;
\d test;
select * from test;

-- type:timestamp format:fileman
drop table if exists test keepdata;
create table test (id integer primary key, dob timestamp(fileman)) global "^datetimetimestampfileman" readonly;
\d test;
select * from test;

-- type:timestamp format:horolog
drop table if exists test keepdata;
create table test (id integer primary key, dob timestamp(horolog)) global "^datetimetimestamphorolog" readonly;
\d test;
select * from test;

-- type:timestamp format:zhorolog
drop table if exists test keepdata;
create table test (id integer primary key, dob timestamp(zhorolog)) global "^datetimetimestampzhorolog" readonly;
\d test;
select * from test;

-- type:timestamp format:zut
drop table if exists test keepdata;
create table test (id integer primary key, dob timestamp(zut)) global "^datetimetimestampzut" readonly;
\d test;
select * from test;


-- type:timestamp with time zone format:
drop table if exists test keepdata;
create table test (id integer primary key, dob timestamp with time zone ) global "^datetimetimestamptz" readonly;
\d test;
select * from test;

-- type:timestamp with time zone format:fileman
drop table if exists test keepdata;
create table test (id integer primary key, dob timestamp(fileman) with time zone) global "^datetimetimestamptzfileman" readonly;
\d test;
select * from test;

-- type:timestamp with time zone format:horolog
drop table if exists test keepdata;
create table test (id integer primary key, dob timestamp(horolog) with time zone) global "^datetimetimestamptzhorolog" readonly;
\d test;
select * from test;

-- type:timestamp with time zone format:zhorolog
drop table if exists test keepdata;
create table test (id integer primary key, dob timestamp(zhorolog) with time zone) global "^datetimetimestamptzzhorolog" readonly;
\d test;
select * from test;

-- type:timestamp with time zone format:zut
drop table if exists test keepdata;
create table test (id integer primary key, dob timestamp(zut) with time zone) global "^datetimetimestamptzzut" readonly;
\d test;
select * from test;


