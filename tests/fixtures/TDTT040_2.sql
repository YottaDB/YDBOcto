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

-- TODO: Enable following `time with time zone`tests below after YDBOcto#1044 is fixed
--(horolog)
-- type:date
drop table if exists test keepdata;
create table test (id integer primary key, dob date(horolog)) global "^datehorolog" readonly;
select * from test;
-- type:time
drop table if exists test keepdata;
create table test (id integer primary key, dob time(horolog)) global "^timehorolog" readonly;
select * from test;
-- type:time with time zone
-- drop table if exists test keepdata;
-- create table test (id integer primary key, dob time(horolog)with time zone) global "^timewithtimezonehorolog" readonly;
-- select * from test;
-- type:timestamp with time zone
drop table if exists test keepdata;
create table test (id integer primary key, dob timestamp(horolog)with time zone) global "^timestampwithtimezonehorolog" readonly;
select * from test;
-- type:timestamp
drop table if exists test keepdata;
create table test (id integer primary key, dob timestamp(horolog)) global "^timestamphorolog" readonly;
select * from test;

--zhorolog
-- type:date
drop table if exists test keepdata;
create table test (id integer primary key, dob date(zhorolog)) global "^datezhorolog" readonly;
select * from test;
-- type:time
drop table if exists test keepdata;
create table test (id integer primary key, dob time(zhorolog)) global "^timezhorolog" readonly;
select * from test;
-- type:time with time zone
-- drop table if exists test keepdata;
-- create table test (id integer primary key, dob time(zhorolog)with time zone) global "^timewithtimezonezhorolog" readonly;
-- select * from test;
-- type:timestamp with time zone
drop table if exists test keepdata;
create table test (id integer primary key, dob timestamp(zhorolog) with time zone) global "^timestampwithtimezonezhorolog" readonly;
select * from test;
-- type:timestamp
drop table if exists test keepdata;
create table test (id integer primary key, dob timestamp(zhorolog)) global "^timestampzhorolog" readonly;
select * from test;

-- fileman
-- type:date
drop table if exists test keepdata;
create table test (id integer primary key, dob date(fileman)) global "^datefileman" readonly;
select * from test;
-- type:time
drop table if exists test keepdata;
create table test (id integer primary key, dob time(fileman)) global "^timefileman" readonly;
select * from test;
-- type:time with time zone
-- drop table if exists test keepdata;
-- create table test (id integer primary key, dob time(fileman)with time zone) global "^timewithtimezonefileman" readonly;
-- select * from test;
-- type:timestamp with time zone
drop table if exists test keepdata;
create table test (id integer primary key, dob timestamp(fileman) with time zone) global "^timestampwithtimezonefileman" readonly;
select * from test;
-- type:timestamp
drop table if exists test keepdata;
create table test (id integer primary key, dob timestamp(fileman)) global "^timestampfileman" readonly;
select * from test;

--zut
-- type:date
drop table if exists test keepdata;
create table test (id integer primary key, dob date(zut)) global "^datezut" readonly;
select * from test;
-- type:time
drop table if exists test keepdata;
create table test (id integer primary key, dob time(zut)) global "^timezut" readonly;
select * from test;
-- type:time with time zone
-- drop table if exists test keepdata;
-- create table test (id integer primary key, dob time(zut)with time zone) global "^timewithtimezonezut" readonly;
-- select * from test;
-- type:timestamp with time zone
drop table if exists test keepdata;
create table test (id integer primary key, dob timestamp(zut)with time zone) global "^timestampwithtimezonezut" readonly;
select * from test;
-- type:timestamp
drop table if exists test keepdata;
create table test (id integer primary key, dob timestamp(zut)) global "^timestampzut" readonly;
select * from test;

--text
-- type:date
drop table if exists test keepdata;
create table test (id integer primary key, dob date) global "^datetext" readonly;
select * from test;
-- type:time
drop table if exists test keepdata;
create table test (id integer primary key, dob time) global "^timetext" readonly;
select * from test;
-- type:time with time zone
-- drop table if exists test keepdata;
-- create table test (id integer primary key, dob time with time zone) global "^timewithtimezonetext" readonly;
-- select * from test;
-- type:timestamp with time zone
drop table if exists test keepdata;
create table test (id integer primary key, dob timestamp with time zone) global "^timestampwithtimezonetext" readonly;
select * from test;
-- type:timestamp
drop table if exists test keepdata;
create table test (id integer primary key, dob timestamp) global "^timestamptext" readonly;
select * from test;
-- ISO 8601
-- type:time
drop table if exists test keepdata;
create table test (id integer primary key, dob time) global "^timetextiso" readonly;
select * from test;
-- type:time with time zone
-- drop table if exists test keepdata;
-- create table test (id integer primary key, dob time with time zone) global "^timewithtimezonetextiso" readonly;
-- select * from test;
-- type:timestamp with time zone
drop table if exists test keepdata;
create table test (id integer primary key, dob timestamp with time zone) global "^timestampwithtimezonetextiso" readonly;
select * from test;
-- type:timestamp
drop table if exists test keepdata;
create table test (id integer primary key, dob timestamp) global "^timestamptextiso" readonly;
select * from test;

-- Edge cases they are not suppose to generate an error
drop table if exists test keepdata;
create table test (id integer primary key, dob timestamp with time zone) global "^edgecase" readonly;
select * from test;
-- ISO 8601
drop table if exists test keepdata;
create table test (id integer primary key, dob timestamp with time zone) global "^edgecaseiso" readonly;
select * from test;
