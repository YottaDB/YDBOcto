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
-- timestamp with time zone
-- text
create table timestamptz_tbl (order_id integer primary key, order_timestamptz timestamp with time zone) GLOBAL "^timestamptzTbl" READONLY;
select * from timestamptz_tbl t1, timestamptz_tbl t2 where t1.order_timestamptz = t2.order_timestamptz;
-- fileman
create table timestamptz_fileman_tbl (order_id integer primary key, order_timestamptz timestamp(fileman) with time zone) GLOBAL "^timestamptzfilemanTbl" READONLY;
select * from timestamptz_fileman_tbl t1, timestamptz_fileman_tbl t2 where t1.order_timestamptz = t2.order_timestamptz;
-- horolog
create table timestamptz_horolog_tbl (order_id integer primary key, order_timestamptz timestamp(horolog) with time zone) GLOBAL "^timestamptzhorologTbl" READONLY;
select * from timestamptz_horolog_tbl t1, timestamptz_horolog_tbl t2 where t1.order_timestamptz = t2.order_timestamptz;
 -- zhorolog
create table timestamptz_zhorolog_tbl (order_id integer primary key, order_timestamptz timestamp(zhorolog) with time zone) GLOBAL "^timestamptzzhorologTbl" READONLY;
select * from timestamptz_zhorolog_tbl t1, timestamptz_zhorolog_tbl t2 where t1.order_timestamptz = t2.order_timestamptz;
-- zut and timestamptz combination is invalid so no test needed

-- time with time zone
-- text
create table timetz_tbl (order_id integer primary key, order_timetz time with time zone) GLOBAL "^timetzTbl" READONLY;
select * from timetz_tbl t1, timetz_tbl t2 where t1.order_timetz = t2.order_timetz;
-- horolog
create table timetz_horolog_tbl (order_id integer primary key, order_timetz time(horolog) with time zone) GLOBAL "^timetzhorologTbl" READONLY;
select * from timetz_horolog_tbl t1, timetz_horolog_tbl t2 where t1.order_timetz = t2.order_timetz;
-- zhorolog
create table timetz_zhorolog_tbl (order_id integer primary key, order_timetz time(zhorolog) with time zone) GLOBAL "^timetzzhorologTbl" READONLY;
select * from timetz_zhorolog_tbl t1, timetz_zhorolog_tbl t2 where t1.order_timetz = t2.order_timetz;
-- zut and fileman with timetz type is invalid so no test needed

-- timestamp
-- text
create table timestamp_tbl (order_id integer primary key, order_timestamp timestamp) GLOBAL "^timestampTbl" READONLY;
select * from timestamp_tbl t1, timestamp_tbl t2 where t1.order_timestamp = t2.order_timestamp;
-- fileman
create table timestamp_fileman_tbl (order_id integer primary key, order_timestamp timestamp(fileman)) GLOBAL "^timestampfilemanTbl" READONLY;
select * from timestamp_fileman_tbl t1, timestamp_fileman_tbl t2 where t1.order_timestamp = t2.order_timestamp;
-- horolog
create table timestamp_horolog_tbl (order_id integer primary key, order_timestamp timestamp(horolog)) GLOBAL "^timestamphorologTbl" READONLY;
select * from timestamp_horolog_tbl t1, timestamp_horolog_tbl t2 where t1.order_timestamp = t2.order_timestamp;
 -- zhorolog
create table timestamp_zhorolog_tbl (order_id integer primary key, order_timestamp timestamp(zhorolog)) GLOBAL "^timestampzhorologTbl" READONLY;
select * from timestamp_zhorolog_tbl t1, timestamp_zhorolog_tbl t2 where t1.order_timestamp = t2.order_timestamp;
 -- zut
create table timestamp_zut_tbl (order_id integer primary key, order_timestamp timestamp(zut)) GLOBAL "^timestampzutTbl" READONLY;
select * from timestamp_zut_tbl t1, timestamp_zut_tbl t2 where t1.order_timestamp = t2.order_timestamp;

-- time
-- text
create table time_tbl (order_id integer primary key, order_time time) GLOBAL "^timeTbl" READONLY;
select * from time_tbl t1, time_tbl t2 where t1.order_time = t2.order_time;
-- horolog
create table time_horolog_tbl (order_id integer primary key, order_time time(horolog)) GLOBAL "^timehorologTbl" READONLY;
select * from time_horolog_tbl t1, time_horolog_tbl t2 where t1.order_time = t2.order_time;
-- zhorolog
create table time_zhorolog_tbl (order_id integer primary key, order_time time(zhorolog)) GLOBAL "^timezhorologTbl" READONLY;
select * from time_zhorolog_tbl t1, time_zhorolog_tbl t2 where t1.order_time = t2.order_time;
-- zut and fileman with time type is invalid so no test needed

-- date
-- text
create table date_tbl (order_id integer primary key, order_date date) GLOBAL "^dateTbl" READONLY;
select * from date_tbl t1, date_tbl t2 where t1.order_date = t2.order_date;
-- fileman
create table date_fileman_tbl (order_id integer primary key, order_date date(fileman)) GLOBAL "^datefilemanTbl" READONLY;
select * from date_fileman_tbl t1, date_fileman_tbl t2 where t1.order_date = t2.order_date;
-- horolog
create table date_horolog_tbl (order_id integer primary key, order_date date(horolog)) GLOBAL "^datehorologTbl" READONLY;
select * from date_horolog_tbl t1, date_horolog_tbl t2 where t1.order_date = t2.order_date;
 -- zhorolog
create table date_zhorolog_tbl (order_id integer primary key, order_date date(zhorolog)) GLOBAL "^datezhorologTbl" READONLY;
select * from date_zhorolog_tbl t1, date_zhorolog_tbl t2 where t1.order_date = t2.order_date;
 -- zut
create table date_zut_tbl (order_id integer primary key, order_date date(zut)) GLOBAL "^datezutTbl" READONLY;
select * from date_zut_tbl t1, date_zut_tbl t2 where t1.order_date = t2.order_date;
