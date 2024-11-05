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
-- TDTT054
select concat(time with time zone'01:01:01-05:00', 'sample text');
select concat('sample text', time with time zone'01:01:01-05:00');
select concat('sample text', date'2023-01-01', time with time zone'01:01:01-05:00');
select concat(date'2023-01-01', 'sample text', time with time zone'01:01:01-05:00');
select concat(date'2023-01-01', time with time zone'01:01:01-05:00', 'sample text');
select concat('sample text', time'01:01:01', time with time zone'01:01:01-05:00');
select concat(time'01:01:01', 'sample text', time with time zone'01:01:01-05:00');
select concat(time'01:01:01', time with time zone'01:01:01-05:00', 'sample text');
select concat('sample text', timestamp'2023-01-01 01:01:01', time with time zone'01:01:01-05:00');
select concat(timestamp'2023-01-01 01:01:01', 'sample text', time with time zone'01:01:01-05:00');
select concat(timestamp'2023-01-01 01:01:01', time with time zone'01:01:01-05:00', 'sample text');
select concat('sample text', time with time zone'01:01:01-05:00', date'2023-01-01');
select concat(time with time zone'01:01:01-05:00', 'sample text', date'2023-01-01');
select concat(time with time zone'01:01:01-05:00', date'2023-01-01', 'sample text');
select concat('sample text', time with time zone'01:01:01-05:00', time'01:01:01');
select concat(time with time zone'01:01:01-05:00', 'sample text', time'01:01:01');
select concat(time with time zone'01:01:01-05:00', time'01:01:01', 'sample text');
select concat('sample text', time with time zone'01:01:01-05:00', timestamp'2023-01-01 01:01:01');
select concat(time with time zone'01:01:01-05:00', 'sample text', timestamp'2023-01-01 01:01:01');
select concat(time with time zone'01:01:01-05:00', timestamp'2023-01-01 01:01:01', 'sample text');
select concat('sample text', time with time zone'01:01:01-05:00', time with time zone'01:01:01-05:00');
select concat(time with time zone'01:01:01-05:00', 'sample text', time with time zone'01:01:01-05:00');
select concat(time with time zone'01:01:01-05:00', time with time zone'01:01:01-05:00', 'sample text');
select concat('sample text', time with time zone'01:01:01-05:00', timestamp with time zone'2023-01-01 01:01:01-05:00');
select concat(time with time zone'01:01:01-05:00', 'sample text', timestamp with time zone'2023-01-01 01:01:01-05:00');
select concat(time with time zone'01:01:01-05:00', timestamp with time zone'2023-01-01 01:01:01-05:00', 'sample text');
select concat('sample text', time with time zone'01:01:01-05:00', 'sample text');
select concat(time with time zone'01:01:01-05:00', 'sample text', 'sample text');
select concat(time with time zone'01:01:01-05:00', 'sample text', 'sample text');
select concat('sample text', timestamp with time zone'2023-01-01 01:01:01-05:00', time with time zone'01:01:01-05:00');
select concat(timestamp with time zone'2023-01-01 01:01:01-05:00', 'sample text', time with time zone'01:01:01-05:00');
select concat(timestamp with time zone'2023-01-01 01:01:01-05:00', time with time zone'01:01:01-05:00', 'sample text');
-- TDTT053
-- mysql test
--select current_time; -- hh:mm:ss no micro second or time zone
-- current_time
-- 6 is used to avoid second values
--select current_time between cast(cast(cast(now() - time'01:01:01') as time) as char(6)) and cast(cast(cast(now() + time'01:01:01') as time) as char(6));
-- Following two are commented because Octo's output will not match mysql's when daylight savings is active
-- select cast(cast(current_time as time) as char(6));
-- select cast(current_time as char(6));

-- TDTT048
-- text to zhorolog validation
-- select time(zhorolog) with time zone'66787,43301,962541,18000';
-- select time(zhorolog) with time zone'66787,43301,,';

-- TDTT040
-- Validate range
select time with time zone'00:00:00.000000-15:59';
select time with time zone'00:00:00.000000+15:59';
select time with time zone'23:59:59.999999-15:59';
select time with time zone'23:59:59.999999+15:59';
--horolog
select time(horolog) with time zone'0';
select time(horolog) with time zone'86399';
-- zhorolog
select time(zhorolog) with time zone',0,,43200';
select time(zhorolog) with time zone',0,0,43200';
select time(zhorolog) with time zone',86399,,-50400';
select time(zhorolog) with time zone',86399,999999,-50400';
-- TDTT034_2
-- first output horolog
-- next outptu text
-- select time(horolog)'3661'::TIME WITH TIME ZONE;
-- select CAST(time(horolog)'3661'AS TIME WITH TIME ZONE);

-- TDTT034_1
-- first output horolog
-- next outptu fileman
-- datetimeoutputformat="horolog"
--select time'01:01:01'::TIME WITH TIME ZONE;
--select time with time zone'01:01:01 -5'::TIME WITH TIME ZONE;
--select CAST(time'01:01:01' AS TIME WITH TIME ZONE);
--select CAST(time with time zone'01:01:01 -5' AS TIME WITH TIME ZONE);

-- TDTT019
select time(horolog) with time zone'56523';
select time(zhorolog) with time zone',56611,,14400';
-- TDTT013
-- type:time with time zone format:
create table TDTT013timetz (id integer, dob time with time zone );
insert into TDTT013timetz values(1,time with time zone'01:01:01+05:00');
create function samevalue(date) returns date as $$samevalue^functions;
drop function samevalue(date);
create function samevalue(time) returns time as $$samevalue^functions;
drop function samevalue(time);
create function samevalue(time with time zone) returns time with time zone as $$samevalue^functions;
select samevalue(time with time zone'01:01:00-05:00');
drop function samevalue(time with time zone);
-- type:time with time zone format:
drop table TDTT013timetz;
-- TDTT011
-- type:time with time zone format:
create table TDTT011timetz (id integer, dob time with time zone );
insert into TDTT011timetz values(1,time with time zone'01:01:01+05:00');
select COALESCE(time with time zone'01:01:00-05:00',time with time zone'01:01:00-05:00');
select GREATEST(time with time zone'01:01:00-05:00',time with time zone'01:01:00-05:00');
select LEAST(time with time zone'01:01:00-05:00',time with time zone'01:01:00-05:00');
select NULLIF(time with time zone'01:01:00-05:00',time with time zone'01:01:00-05:00');
select COALESCE(time with time zone'01:01:00-05:00',n1.dob) from TDTT011timetz n1;
select GREATEST(time with time zone'01:01:00-05:00',n1.dob) from TDTT011timetz n1;
select LEAST(time with time zone'01:01:00-05:00',n1.dob) from TDTT011timetz n1;
select NULLIF(time with time zone'01:01:00-05:00',n1.dob) from TDTT011timetz n1;
select COALESCE(time with time zone'01:01:00-05:00',NULL);
select GREATEST(time with time zone'01:01:00-05:00',NULL);
select LEAST(time with time zone'01:01:00-05:00',NULL);
select NULLIF(time with time zone'01:01:00-05:00',NULL);
select COALESCE(n1.dob,time with time zone'01:01:00-05:00') from TDTT011timetz n1;
select GREATEST(n1.dob,time with time zone'01:01:00-05:00') from TDTT011timetz n1;
select LEAST(n1.dob,time with time zone'01:01:00-05:00') from TDTT011timetz n1;
select NULLIF(n1.dob,time with time zone'01:01:00-05:00') from TDTT011timetz n1;
-- type:time with time zone format:
drop table TDTT011timetz;
-- TDTT010
-- type:time with time zone format:
create table TDTT010timetz (id integer, dob time with time zone );
insert into TDTT010timetz values(1,time with time zone'01:01:01+05:00');
select COUNT(time with time zone'01:01:00-05:00');
select COUNT(DISTINCT time with time zone'01:01:00-05:00');
select COUNT(ALL time with time zone'01:01:00-05:00');
select MIN(time with time zone'01:01:00-05:00');
select MIN(DISTINCT time with time zone'01:01:00-05:00');
select MIN(ALL time with time zone'01:01:00-05:00');
select MAX(time with time zone'01:01:00-05:00');
select MAX(DISTINCT time with time zone'01:01:00-05:00');
select MAX(ALL time with time zone'01:01:00-05:00');
-- type:time with time zone format:
drop table TDTT010timetz;
-- TDTT009
-- type:time with time zone format:
create table TDTT009timetz (id integer, dob time with time zone );
insert into TDTT009timetz values(1,time with time zone'01:01:01+05:00');
select ARRAY(select time with time zone'01:01:00-05:00');
select ARRAY(values (time with time zone'01:01:00-05:00'));
-- type:time with time zone format:
drop table TDTT009timetz;
-- TDTT001
-- TDTT001.sql run through cross check
select time with time zone'16:12:00';
-- result should be same but with time zone of -5
-- TDTT001_octo.sql
-- run through load_fixture
-- Following query's result difference is because of direct use of time.h. The time
--   is considered to be present day's time and hence is being considered to local time for the day.
-- Note: Mysql doesn't allow timezone information in time type hence doesn't have this problem
select time with time zone'22:30:00+05:30';
-- Postgres result is '22:30:00+05:30' but Octo result is 12:00:00-0500
select time with time zone'10:30:00+05:30';
-- This works in postgres but octo will say invalid format

select time with time zone '12:01:41.962541-05:00';
-- TDTT002
-- echo "datestyle = \"YMD\"" > octo.conf
-- test_port=$(start_rocto 1400 -aw)
-- Time with time zone
-- HH:MM:SS-/+HHMM
-- with timezone
drop table if exists test3wt;
create table test3wt (id int, tob time with time zone);
insert into test3wt values(1, time with time zone'11:00:00');
-- Following query is commented because of difference in output seen in Postgres and Octo.
-- This is tracked by 8 in https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/382#note_1646532513.
-- 	insert into test3wt values(2, time with time zone'11:00:00-0830');
-- Following is commented as the timezone information will change with daylight saving and the result
-- seen will vary in Octo and Postgres because of the same reason mentioned in previous comment
-- 	insert into test3wt values(1, time with time zone'11:00:00-0500');
select * from test3wt;
-- TDTT003
-- load_fixture
-- type:time with time zone format:
drop table if exists test;
create table test (id integer, dob time with time zone );
\d test;
insert into test values(1,time with time zone'01:01:01-05:00');
insert into test values(2,time with time zone'01:02:01-05:00');
select * from test;
-- type:time with time zone format:fileman
drop table if exists test;
-- type:time with time zone format:horolog
drop table if exists test;
create table test (id integer, dob time(horolog) with time zone);
\d test;
insert into test values(1,time with time zone'01:01:01-05:00');
insert into test values(2,time with time zone'01:02:01-05:00');
select * from test;
drop table if exists test;
create table test (id integer, dob time(zhorolog) with time zone);
\d test;
insert into test values(1,time with time zone'01:01:01-05:00');
insert into test values(2,time with time zone'01:02:01-05:00');
select * from test;
-- type:time with time zone format:zut
drop table if exists test;
-- TDTT004
-- loadfixture
-- $ydb_dist/yottadb -r datetimemglobal^datetime
-- type:time with time zone format:
drop table if exists test keepdata;
create table test (id integer primary key, dob time with time zone ) global "^datetimetimetz" readonly;
\d test;
select * from test;
-- type:time with time zone format:fileman
drop table if exists test keepdata;
-- type:time with time zone format:horolog
drop table if exists test keepdata;
create table test (id integer primary key, dob time(horolog) with time zone) global "^datetimetimetzhorolog" readonly;
\d test;
select * from test;
-- type:time with time zone format:zhorolog
drop table if exists test keepdata;
create table test (id integer primary key, dob time(zhorolog) with time zone) global "^datetimetimetzzhorolog" readonly;
\d test;
select * from test;
-- type:time with time zone format:zut
drop table if exists test keepdata;
-- TDTT006
-- type:time with time zone format:
create table TDTT006timetz (id integer, dob time with time zone );
insert into TDTT006timetz values(1,time with time zone'01:01:01+05:00');
select EXISTS(select dob from TDTT006timetz n1);
drop table TDTT006timetz;
-- TDTT007
-- type:time with time zone format:
create table TDTT005timetz (id integer, dob time with time zone );
insert into TDTT005timetz values(1,time with time zone'01:01:01+05:00');
drop table TDTT005timetz;
select time with time zone'01:01:00-05:00'::TIME;
select CAST(time with time zone'01:01:00-05:00' AS TIME);
select time with time zone'01:01:00-05:00'::TIME WITH TIME ZONE;
select CAST(time with time zone'01:01:00-05:00' AS TIME WITH TIME ZONE);
select time with time zone'01:01:00-05:00'::VARCHAR;
select CAST(time with time zone'01:01:00-05:00' AS VARCHAR);
select time'01:01:00'::TIME WITH TIME ZONE;
select CAST(time'01:01:00' AS TIME WITH TIME ZONE);
-- type:date format:
create table TDTT005date (id integer, dob date );
insert into TDTT005date values(1,date'2023-01-01');
-- type:time format:
create table TDTT005time (id integer, dob time );
insert into TDTT005time values(1,time'01:01:01');
-- type:time with time zone format:
create table TDTT005timetz (id integer, dob time with time zone );
insert into TDTT005timetz values(1,time with time zone'01:01:01+05:00');
-- type:timestamp format:
create table TDTT005timestamp (id integer, dob timestamp );
insert into TDTT005timestamp values(1,timestamp'2023-01-01 01:01:01');
-- type:timestamp with time zone format:
create table TDTT005timestamptz (id integer, dob timestamp with time zone );
insert into TDTT005timestamptz values(1,timestamp with time zone'2023-01-01 01:01:01+05:00');
select dob::TIME WITH TIME ZONE from TDTT005time n1;
select CAST(dob AS TIME WITH TIME ZONE) from TDTT005time n1;
select dob::TIME WITH TIME ZONE from TDTT005timetz n1;
select CAST(dob AS TIME WITH TIME ZONE) from TDTT005timetz n1;
drop table TDTT005timetz;
-- type:date format:
drop table TDTT005date;
-- type:time format:
drop table TDTT005time;
-- type:timestamp format:
drop table TDTT005timestamp;
-- type:timestamp with time zone format:
drop table TDTT005timestamptz;
-- TDTT008
-- type:date format:
create table TDTT008date (id integer, dob date );
insert into TDTT008date values(1,date'2023-01-01');
-- type:time format:
create table TDTT008time (id integer, dob time );
insert into TDTT008time values(1,time'01:01:01');
-- type:timestamp format:
create table TDTT008timestamp (id integer, dob timestamp );
insert into TDTT008timestamp values(1,timestamp'2023-01-01 01:01:01');
-- type:timestamp with time zone format:
create table TDTT008timestamptz (id integer, dob timestamp with time zone );
insert into TDTT008timestamptz values(1,timestamp with time zone'2023-01-01 01:01:01+05:00');
create table TDTT008timetz (id integer, dob time with time zone);
insert into TDTT008timetz values (1,time with time zone'01:01:01-05');
select CASE time with time zone'01:01:00-05:00' WHEN n1.dob THEN time with time zone'01:01:00-05:00' ELSE n1.dob END from TDTT008timetz n1;
select CASE WHEN time with time zone'01:01:00-05:00' = n1.dob THEN time with time zone'01:01:00-05:00' ELSE n1.dob END from TDTT008timetz n1;
select CASE n1.dob WHEN time with time zone'01:01:00-05:00' THEN n1.dob ELSE time with time zone'01:01:00-05:00' END from TDTT008timetz n1;
select CASE WHEN n1.dob = time with time zone'01:01:00-05:00' THEN n1.dob ELSE time with time zone'01:01:00-05:00' END from TDTT008timetz n1;
select CASE n1.dob WHEN n2.dob THEN n1.dob ELSE n2.dob END from TDTT008timetz n1, TDTT008timetz n2;
select CASE WHEN n1.dob = n2.dob THEN n1.dob ELSE n2.dob END from TDTT008timetz n1, TDTT008timetz n2;
select CASE n1.dob WHEN NULL THEN n1.dob ELSE NULL END from TDTT008timetz n1;
select CASE WHEN n1.dob = NULL THEN n1.dob ELSE NULL END from TDTT008timetz n1;
select CASE NULL WHEN n1.dob THEN NULL ELSE n1.dob END from TDTT008timetz n1;
drop table TDTT008date;
drop table TDTT008time;
drop table TDTT008timetz;
drop table TDTT008timestamp;
drop table TDTT008timestamptz;
create table TDTT005date (id integer, dob date );
insert into TDTT005date values(1,date'2023-01-01');
create table TDTT005time (id integer, dob time );
insert into TDTT005time values(1,time'01:01:01');
-- type:time with time zone format:
create table TDTT005timetz (id integer, dob time with time zone );
insert into TDTT005timetz values(1,time with time zone'01:01:01+05:00');
create table TDTT005timestamp (id integer, dob timestamp );
insert into TDTT005timestamp values(1,timestamp'2023-01-01 01:01:01');
create table TDTT005timestamptz (id integer, dob timestamp with time zone );
insert into TDTT005timestamptz values(1,timestamp with time zone'2023-01-01 01:01:01+05:00');
select time with time zone'01:01:00-05:00' + n1.dob from TDTT005date n1;
select time with time zone'01:01:00-05:00' + n1.dob from TDTT005time n1;
select n1.dob + time with time zone'01:01:00-05:00' from TDTT005date n1;
select n1.dob + time with time zone'01:01:00-05:00' from TDTT005time n1;
select time with time zone'01:01:00-05:00' BETWEEN n1.dob AND time with time zone'01:01:00-05:00' from TDTT005time n1;
select time with time zone'01:01:00-05:00' BETWEEN n1.dob AND time with time zone'01:01:00-05:00' from TDTT005timetz n1;
select n1.dob BETWEEN time with time zone'01:01:00-05:00' AND n1.dob from TDTT005time n1;
select n1.dob BETWEEN time with time zone'01:01:00-05:00' AND n1.dob from TDTT005timetz n1;
select time with time zone'01:01:00-05:00' = n1.dob from TDTT005time n1;
select time with time zone'01:01:00-05:00' = ANY(select dob from TDTT005time);
select time with time zone'01:01:00-05:00' = ALL(select dob from TDTT005time);
select time with time zone'01:01:00-05:00' != n1.dob from TDTT005time n1;
select time with time zone'01:01:00-05:00' != ANY(select dob from TDTT005time);
select time with time zone'01:01:00-05:00' != ALL(select dob from TDTT005time);
select time with time zone'01:01:00-05:00' < n1.dob from TDTT005time n1;
select time with time zone'01:01:00-05:00' < ANY(select dob from TDTT005time);
select time with time zone'01:01:00-05:00' < ALL(select dob from TDTT005time);
select time with time zone'01:01:00-05:00' > n1.dob from TDTT005time n1;
select time with time zone'01:01:00-05:00' > ANY(select dob from TDTT005time);
select time with time zone'01:01:00-05:00' > ALL(select dob from TDTT005time);
select time with time zone'01:01:00-05:00' <= n1.dob from TDTT005time n1;
select time with time zone'01:01:00-05:00' <= ANY(select dob from TDTT005time);
select time with time zone'01:01:00-05:00' <= ALL(select dob from TDTT005time);
select time with time zone'01:01:00-05:00' >= n1.dob from TDTT005time n1;
select time with time zone'01:01:00-05:00' >= ANY(select dob from TDTT005time);
select time with time zone'01:01:00-05:00' >= ALL(select dob from TDTT005time);
select n1.dob = time with time zone'01:01:00-05:00' from TDTT005time n1;
select n1.dob = ANY(select time with time zone'01:01:00-05:00') from TDTT005time n1;
select n1.dob = ALL(select time with time zone'01:01:00-05:00') from TDTT005time n1;
select n1.dob != time with time zone'01:01:00-05:00' from TDTT005time n1;
select n1.dob != ANY(select time with time zone'01:01:00-05:00') from TDTT005time n1;
select n1.dob != ALL(select time with time zone'01:01:00-05:00') from TDTT005time n1;
select n1.dob < time with time zone'01:01:00-05:00' from TDTT005time n1;
select n1.dob < ANY(select time with time zone'01:01:00-05:00') from TDTT005time n1;
select n1.dob < ALL(select time with time zone'01:01:00-05:00') from TDTT005time n1;
select n1.dob > time with time zone'01:01:00-05:00' from TDTT005time n1;
select n1.dob > ANY(select time with time zone'01:01:00-05:00') from TDTT005time n1;
select n1.dob > ALL(select time with time zone'01:01:00-05:00') from TDTT005time n1;
select n1.dob <= time with time zone'01:01:00-05:00' from TDTT005time n1;
select n1.dob <= ANY(select time with time zone'01:01:00-05:00') from TDTT005time n1;
select n1.dob <= ALL(select time with time zone'01:01:00-05:00') from TDTT005time n1;
select n1.dob >= time with time zone'01:01:00-05:00' from TDTT005time n1;
select n1.dob >= ANY(select time with time zone'01:01:00-05:00') from TDTT005time n1;
select n1.dob >= ALL(select time with time zone'01:01:00-05:00') from TDTT005time n1;
select date'2023-01-01' + time with time zone'01:01:00-05:00';
select time'01:01:00' + time with time zone'01:01:00-05:00';
select time with time zone'01:01:00-05:00' + date'2023-01-01';
select time with time zone'01:01:00-05:00' + time'01:01:00';
select time'01:01:00' BETWEEN time with time zone'01:01:00-05:00' AND time'01:01:00';
select time with time zone'01:01:00-05:00' BETWEEN time'01:01:00' AND time with time zone'01:01:00-05:00';
select time with time zone'01:01:00-05:00' BETWEEN time with time zone'01:01:00-05:00' AND time with time zone'01:01:00-05:00';
select time with time zone'01:01:00-05:00' BETWEEN NULL AND time with time zone'01:01:00-05:00';
select time'01:01:00' = time with time zone'01:01:00-05:00';
select time'01:01:00' = ANY(select time with time zone'01:01:00-05:00');
select time'01:01:00' = ALL(select time with time zone'01:01:00-05:00');
select time'01:01:00' != time with time zone'01:01:00-05:00';
select time'01:01:00' != ANY(select time with time zone'01:01:00-05:00');
select time'01:01:00' != ALL(select time with time zone'01:01:00-05:00');
select time'01:01:00' < time with time zone'01:01:00-05:00';
select time'01:01:00' < ANY(select time with time zone'01:01:00-05:00');
select time'01:01:00' < ALL(select time with time zone'01:01:00-05:00');
select time'01:01:00' > time with time zone'01:01:00-05:00';
select time'01:01:00' > ANY(select time with time zone'01:01:00-05:00');
select time'01:01:00' > ALL(select time with time zone'01:01:00-05:00');
select time'01:01:00' <= time with time zone'01:01:00-05:00';
select time'01:01:00' <= ANY(select time with time zone'01:01:00-05:00');
select time'01:01:00' <= ALL(select time with time zone'01:01:00-05:00');
select time'01:01:00' >= time with time zone'01:01:00-05:00';
select time'01:01:00' >= ANY(select time with time zone'01:01:00-05:00');
select time'01:01:00' >= ALL(select time with time zone'01:01:00-05:00');
select time with time zone'01:01:00-05:00' = time'01:01:00';
select time with time zone'01:01:00-05:00' = ANY(select time'01:01:00');
select time with time zone'01:01:00-05:00' = ALL(select time'01:01:00');
select time with time zone'01:01:00-05:00' != time'01:01:00';
select time with time zone'01:01:00-05:00' != ANY(select time'01:01:00');
select time with time zone'01:01:00-05:00' != ALL(select time'01:01:00');
select time with time zone'01:01:00-05:00' < time'01:01:00';
select time with time zone'01:01:00-05:00' < ANY(select time'01:01:00');
select time with time zone'01:01:00-05:00' < ALL(select time'01:01:00');
select time with time zone'01:01:00-05:00' > time'01:01:00';
select time with time zone'01:01:00-05:00' > ANY(select time'01:01:00');
select time with time zone'01:01:00-05:00' > ALL(select time'01:01:00');
select time with time zone'01:01:00-05:00' <= time'01:01:00';
select time with time zone'01:01:00-05:00' <= ANY(select time'01:01:00');
select time with time zone'01:01:00-05:00' <= ALL(select time'01:01:00');
select time with time zone'01:01:00-05:00' >= time'01:01:00';
select time with time zone'01:01:00-05:00' >= ANY(select time'01:01:00');
select time with time zone'01:01:00-05:00' >= ALL(select time'01:01:00');
select time with time zone'01:01:00-05:00' = time with time zone'01:01:00-05:00';
select time with time zone'01:01:00-05:00' = ANY(select time with time zone'01:01:00-05:00');
select time with time zone'01:01:00-05:00' = ALL(select time with time zone'01:01:00-05:00');
select time with time zone'01:01:00-05:00' != time with time zone'01:01:00-05:00';
select time with time zone'01:01:00-05:00' != ANY(select time with time zone'01:01:00-05:00');
select time with time zone'01:01:00-05:00' != ALL(select time with time zone'01:01:00-05:00');
select time with time zone'01:01:00-05:00' < time with time zone'01:01:00-05:00';
select time with time zone'01:01:00-05:00' < ANY(select time with time zone'01:01:00-05:00');
select time with time zone'01:01:00-05:00' < ALL(select time with time zone'01:01:00-05:00');
select time with time zone'01:01:00-05:00' > time with time zone'01:01:00-05:00';
select time with time zone'01:01:00-05:00' > ANY(select time with time zone'01:01:00-05:00');
select time with time zone'01:01:00-05:00' > ALL(select time with time zone'01:01:00-05:00');
select time with time zone'01:01:00-05:00' <= time with time zone'01:01:00-05:00';
select time with time zone'01:01:00-05:00' <= ANY(select time with time zone'01:01:00-05:00');
select time with time zone'01:01:00-05:00' <= ALL(select time with time zone'01:01:00-05:00');
select time with time zone'01:01:00-05:00' >= time with time zone'01:01:00-05:00';
select time with time zone'01:01:00-05:00' >= ANY(select time with time zone'01:01:00-05:00');
select time with time zone'01:01:00-05:00' >= ALL(select time with time zone'01:01:00-05:00');
select time with time zone'01:01:00-05:00' = n1.dob from TDTT005timetz n1;
select time with time zone'01:01:00-05:00' = ALL(select dob from TDTT005timetz);
select time with time zone'01:01:00-05:00' != n1.dob from TDTT005timetz n1;
select time with time zone'01:01:00-05:00' != ALL(select dob from TDTT005timetz);
select time with time zone'01:01:00-05:00' < n1.dob from TDTT005timetz n1;
select time with time zone'01:01:00-05:00' < ALL(select dob from TDTT005timetz);
select time with time zone'01:01:00-05:00' > n1.dob from TDTT005timetz n1;
select time with time zone'01:01:00-05:00' > ALL(select dob from TDTT005timetz);
select time with time zone'01:01:00-05:00' <= n1.dob from TDTT005timetz n1;
select time with time zone'01:01:00-05:00' <= ALL(select dob from TDTT005timetz);
select time with time zone'01:01:00-05:00' >= n1.dob from TDTT005timetz n1;
select time with time zone'01:01:00-05:00' >= ALL(select dob from TDTT005timetz);
select time with time zone'01:01:00-05:00' = NULL;
select time with time zone'01:01:00-05:00' = ANY(select NULL);
select time with time zone'01:01:00-05:00' = ALL(select NULL);
select time with time zone'01:01:00-05:00' != NULL;
select time with time zone'01:01:00-05:00' != ANY(select NULL);
select time with time zone'01:01:00-05:00' != ALL(select NULL);
select time with time zone'01:01:00-05:00' < NULL;
select time with time zone'01:01:00-05:00' < ANY(select NULL);
select time with time zone'01:01:00-05:00' < ALL(select NULL);
select time with time zone'01:01:00-05:00' > NULL;
select time with time zone'01:01:00-05:00' > ANY(select NULL);
select time with time zone'01:01:00-05:00' > ALL(select NULL);
select time with time zone'01:01:00-05:00' <= NULL;
select time with time zone'01:01:00-05:00' <= ANY(select NULL);
select time with time zone'01:01:00-05:00' <= ALL(select NULL);
select time with time zone'01:01:00-05:00' >= NULL;
select time with time zone'01:01:00-05:00' >= ANY(select NULL);
select time with time zone'01:01:00-05:00' >= ALL(select NULL);
select n1.dob = time with time zone'01:01:00-05:00' from TDTT005timetz n1;
select time'01:01:00' IN (select time with time zone'01:01:00-05:00');
select time with time zone'01:01:00-05:00' IN (select time'01:01:00');
select time with time zone'01:01:00-05:00' IN (time with time zone'01:01:00-05:00',time with time zone'01:01:00-05:00');
select time with time zone'01:01:00-05:00' IN (select time with time zone'01:01:00-05:00');
select time with time zone'01:01:00-05:00' IN (select n1.dob) from TDTT005time n1;
select time with time zone'01:01:00-05:00' IN (n1.dob,time with time zone'01:01:00-05:00') from TDTT005timetz n1;
select time with time zone'01:01:00-05:00' IN (select n1.dob) from TDTT005timetz n1;
select time with time zone'01:01:00-05:00' IN (NULL,time with time zone'01:01:00-05:00');
select time with time zone'01:01:00-05:00' IN (select NULL);
select n1.dob IN (select time with time zone'01:01:00-05:00') from TDTT005time n1;
select n1.dob IN (time with time zone'01:01:00-05:00',n1.dob) from TDTT005timetz n1;
select n1.dob IN (select time with time zone'01:01:00-05:00') from TDTT005timetz n1;
select time'01:01:00' NOT BETWEEN time with time zone'01:01:00-05:00' AND time'01:01:00';
select time with time zone'01:01:00-05:00' NOT BETWEEN time'01:01:00' AND time with time zone'01:01:00-05:00';
select time with time zone'01:01:00-05:00' NOT BETWEEN time with time zone'01:01:00-05:00' AND time with time zone'01:01:00-05:00';
select time with time zone'01:01:00-05:00' NOT BETWEEN n1.dob AND time with time zone'01:01:00-05:00' from TDTT005time n1;
select time with time zone'01:01:00-05:00' NOT BETWEEN n1.dob AND time with time zone'01:01:00-05:00' from TDTT005timetz n1;
select n1.dob NOT BETWEEN time with time zone'01:01:00-05:00' AND n1.dob from TDTT005time n1;
select n1.dob NOT BETWEEN time with time zone'01:01:00-05:00' AND n1.dob from TDTT005timetz n1;
select time'01:01:00' NOT IN (select time with time zone'01:01:00-05:00');
select time with time zone'01:01:00-05:00' NOT IN (select time'01:01:00');
select time with time zone'01:01:00-05:00' NOT IN (time with time zone'01:01:00-05:00',time with time zone'01:01:00-05:00');
select time with time zone'01:01:00-05:00' NOT IN (select time with time zone'01:01:00-05:00');
select time with time zone'01:01:00-05:00' NOT IN (select n1.dob) from TDTT005time n1;
select time with time zone'01:01:00-05:00' NOT IN (n1.dob,time with time zone'01:01:00-05:00') from TDTT005timetz n1;
select time with time zone'01:01:00-05:00' NOT IN (select n1.dob) from TDTT005timetz n1;
select time with time zone'01:01:00-05:00' NOT IN (NULL,time with time zone'01:01:00-05:00');
select time with time zone'01:01:00-05:00' NOT IN (select NULL);
select time with time zone'01:01:00-05:00' NOT IN (NULL,time with time zone'01:01:00-05:00');
select time with time zone'01:01:00-05:00' NOT IN (select NULL);
select n1.dob NOT IN (select time with time zone'01:01:00-05:00') from TDTT005time n1;
select n1.dob NOT IN (time with time zone'01:01:00-05:00',n1.dob) from TDTT005timetz n1;
select n1.dob NOT IN (select time with time zone'01:01:00-05:00') from TDTT005timetz n1;
select time with time zone'01:01:00-05:00' - NULL;
select date'2023-01-01' + n1.dob from TDTT005timetz n1;
select time'01:01:00' + n1.dob from TDTT005timetz n1;
select n1.dob + n2.dob from TDTT005date n1, TDTT005timetz n2;
select n1.dob + n2.dob from TDTT005time n1, TDTT005timetz n2;
select n1.dob + date'2023-01-01' from TDTT005timetz n1;
select n1.dob + time'01:01:00' from TDTT005timetz n1;
select n1.dob + n2.dob from TDTT005timetz n1, TDTT005date n2;
select n1.dob + n2.dob from TDTT005timetz n1, TDTT005time n2;
select time'01:01:00' BETWEEN n1.dob AND time'01:01:00' from TDTT005timetz n1;
select n1.dob BETWEEN n2.dob AND n1.dob from TDTT005time n1, TDTT005timetz n2;
select n1.dob BETWEEN time'01:01:00' AND n1.dob from TDTT005timetz n1;
select n1.dob BETWEEN n2.dob AND n1.dob from TDTT005timetz n1, TDTT005time n2;
select n1.dob BETWEEN n2.dob AND n1.dob from TDTT005timetz n1, TDTT005timetz n2;
select n1.dob BETWEEN NULL AND n1.dob from TDTT005timetz n1;
select time'01:01:00' = n1.dob from TDTT005timetz n1;
select time'01:01:00' = ANY(select dob from TDTT005timetz);
select time'01:01:00' = ALL(select dob from TDTT005timetz);
select time'01:01:00' != n1.dob from TDTT005timetz n1;
select time'01:01:00' != ANY(select dob from TDTT005timetz);
select time'01:01:00' != ALL(select dob from TDTT005timetz);
select time'01:01:00' < n1.dob from TDTT005timetz n1;
select time'01:01:00' < ANY(select dob from TDTT005timetz);
select time'01:01:00' < ALL(select dob from TDTT005timetz);
select time'01:01:00' > n1.dob from TDTT005timetz n1;
select time'01:01:00' > ANY(select dob from TDTT005timetz);
select time'01:01:00' > ALL(select dob from TDTT005timetz);
select time'01:01:00' <= n1.dob from TDTT005timetz n1;
select time'01:01:00' <= ANY(select dob from TDTT005timetz);
select time'01:01:00' <= ALL(select dob from TDTT005timetz);
select time'01:01:00' >= n1.dob from TDTT005timetz n1;
select time'01:01:00' >= ANY(select dob from TDTT005timetz);
select time'01:01:00' >= ALL(select dob from TDTT005timetz);
select n1.dob = n2.dob from TDTT005time n1, TDTT005timetz n2;
select n1.dob = ANY(select dob from TDTT005timetz) from TDTT005time n1;
select n1.dob = ALL(select dob from TDTT005timetz) from TDTT005time n1;
select n1.dob != n2.dob from TDTT005time n1, TDTT005timetz n2;
select n1.dob != ANY(select dob from TDTT005timetz) from TDTT005time n1;
select n1.dob != ALL(select dob from TDTT005timetz) from TDTT005time n1;
select n1.dob < n2.dob from TDTT005time n1, TDTT005timetz n2;
select n1.dob < ANY(select dob from TDTT005timetz) from TDTT005time n1;
select n1.dob < ALL(select dob from TDTT005timetz) from TDTT005time n1;
select n1.dob > n2.dob from TDTT005time n1, TDTT005timetz n2;
select n1.dob > ANY(select dob from TDTT005timetz) from TDTT005time n1;
select n1.dob > ALL(select dob from TDTT005timetz) from TDTT005time n1;
select n1.dob <= n2.dob from TDTT005time n1, TDTT005timetz n2;
select n1.dob <= ANY(select dob from TDTT005timetz) from TDTT005time n1;
select n1.dob <= ALL(select dob from TDTT005timetz) from TDTT005time n1;
select n1.dob >= n2.dob from TDTT005time n1, TDTT005timetz n2;
select n1.dob >= ANY(select dob from TDTT005timetz) from TDTT005time n1;
select n1.dob >= ALL(select dob from TDTT005timetz) from TDTT005time n1;
select n1.dob = time'01:01:00' from TDTT005timetz n1;
select n1.dob = ANY(select time'01:01:00') from TDTT005timetz n1;
select n1.dob = ALL(select time'01:01:00') from TDTT005timetz n1;
select n1.dob != time'01:01:00' from TDTT005timetz n1;
select n1.dob != ANY(select time'01:01:00') from TDTT005timetz n1;
select n1.dob != ALL(select time'01:01:00') from TDTT005timetz n1;
select n1.dob < time'01:01:00' from TDTT005timetz n1;
select n1.dob < ANY(select time'01:01:00') from TDTT005timetz n1;
select n1.dob < ALL(select time'01:01:00') from TDTT005timetz n1;
select n1.dob > time'01:01:00' from TDTT005timetz n1;
select n1.dob > ANY(select time'01:01:00') from TDTT005timetz n1;
select n1.dob > ALL(select time'01:01:00') from TDTT005timetz n1;
select n1.dob <= time'01:01:00' from TDTT005timetz n1;
select n1.dob <= ANY(select time'01:01:00') from TDTT005timetz n1;
select n1.dob <= ALL(select time'01:01:00') from TDTT005timetz n1;
select n1.dob >= time'01:01:00' from TDTT005timetz n1;
select n1.dob >= ANY(select time'01:01:00') from TDTT005timetz n1;
select n1.dob >= ALL(select time'01:01:00') from TDTT005timetz n1;
select n1.dob = n2.dob from TDTT005timetz n1, TDTT005time n2;
select n1.dob = ANY(select dob from TDTT005time) from TDTT005timetz n1;
select n1.dob = ALL(select dob from TDTT005time) from TDTT005timetz n1;
select n1.dob != n2.dob from TDTT005timetz n1, TDTT005time n2;
select n1.dob != ANY(select dob from TDTT005time) from TDTT005timetz n1;
select n1.dob != ALL(select dob from TDTT005time) from TDTT005timetz n1;
select n1.dob < n2.dob from TDTT005timetz n1, TDTT005time n2;
select n1.dob < ANY(select dob from TDTT005time) from TDTT005timetz n1;
select n1.dob < ALL(select dob from TDTT005time) from TDTT005timetz n1;
select n1.dob > n2.dob from TDTT005timetz n1, TDTT005time n2;
select n1.dob > ANY(select dob from TDTT005time) from TDTT005timetz n1;
select n1.dob > ALL(select dob from TDTT005time) from TDTT005timetz n1;
select n1.dob <= n2.dob from TDTT005timetz n1, TDTT005time n2;
select n1.dob <= ANY(select dob from TDTT005time) from TDTT005timetz n1;
select n1.dob <= ALL(select dob from TDTT005time) from TDTT005timetz n1;
select n1.dob >= n2.dob from TDTT005timetz n1, TDTT005time n2;
select n1.dob >= ANY(select dob from TDTT005time) from TDTT005timetz n1;
select n1.dob >= ALL(select dob from TDTT005time) from TDTT005timetz n1;
select n1.dob = n2.dob from TDTT005timetz n1, TDTT005timetz n2;
select n1.dob = ANY(select dob from TDTT005timetz) from TDTT005timetz n1;
select n1.dob = ALL(select dob from TDTT005timetz) from TDTT005timetz n1;
select n1.dob != n2.dob from TDTT005timetz n1, TDTT005timetz n2;
select n1.dob != ANY(select dob from TDTT005timetz) from TDTT005timetz n1;
select n1.dob != ALL(select dob from TDTT005timetz) from TDTT005timetz n1;
select n1.dob < n2.dob from TDTT005timetz n1, TDTT005timetz n2;
select n1.dob < ANY(select dob from TDTT005timetz) from TDTT005timetz n1;
select n1.dob < ALL(select dob from TDTT005timetz) from TDTT005timetz n1;
select n1.dob > n2.dob from TDTT005timetz n1, TDTT005timetz n2;
select n1.dob > ANY(select dob from TDTT005timetz) from TDTT005timetz n1;
select n1.dob > ALL(select dob from TDTT005timetz) from TDTT005timetz n1;
select n1.dob <= n2.dob from TDTT005timetz n1, TDTT005timetz n2;
select n1.dob <= ANY(select dob from TDTT005timetz) from TDTT005timetz n1;
select n1.dob <= ALL(select dob from TDTT005timetz) from TDTT005timetz n1;
select n1.dob >= n2.dob from TDTT005timetz n1, TDTT005timetz n2;
select n1.dob >= ANY(select dob from TDTT005timetz) from TDTT005timetz n1;
select n1.dob >= ALL(select dob from TDTT005timetz) from TDTT005timetz n1;
select n1.dob = NULL from TDTT005timetz n1;
select n1.dob = ANY(select NULL) from TDTT005timetz n1;
select n1.dob = ALL(select NULL) from TDTT005timetz n1;
select n1.dob != NULL from TDTT005timetz n1;
select n1.dob != ANY(select NULL) from TDTT005timetz n1;
select n1.dob != ALL(select NULL) from TDTT005timetz n1;
select n1.dob < NULL from TDTT005timetz n1;
select n1.dob < ANY(select NULL) from TDTT005timetz n1;
select n1.dob < ALL(select NULL) from TDTT005timetz n1;
select n1.dob > NULL from TDTT005timetz n1;
select n1.dob > ANY(select NULL) from TDTT005timetz n1;
select n1.dob > ALL(select NULL) from TDTT005timetz n1;
select n1.dob <= NULL from TDTT005timetz n1;
select n1.dob <= ANY(select NULL) from TDTT005timetz n1;
select n1.dob <= ALL(select NULL) from TDTT005timetz n1;
select n1.dob >= NULL from TDTT005timetz n1;
select n1.dob >= ANY(select NULL) from TDTT005timetz n1;
select n1.dob >= ALL(select NULL) from TDTT005timetz n1;
select n1.dob || 'sample string' from TDTT005timetz n1;
select n1.dob || NULL from TDTT005timetz n1;
select time'01:01:00' IN (select n1.dob) from TDTT005timetz n1;
select n1.dob IN (select n2.dob) from TDTT005time n1, TDTT005timetz n2;
select n1.dob IN (select time'01:01:00') from TDTT005timetz n1;
select n1.dob IN (select n2.dob) from TDTT005timetz n1, TDTT005time n2;
select n1.dob IN (n2.dob,n1.dob) from TDTT005timetz n1, TDTT005timetz n2;
select n1.dob IN (select n2.dob) from TDTT005timetz n1, TDTT005timetz n2;
select n1.dob IN (NULL,n1.dob) from TDTT005timetz n1;
select n1.dob IN (select NULL) from TDTT005timetz n1;
select time'01:01:00' NOT BETWEEN n1.dob AND time'01:01:00' from TDTT005timetz n1;
select n1.dob NOT BETWEEN n2.dob AND n1.dob from TDTT005time n1, TDTT005timetz n2;
select n1.dob NOT BETWEEN time'01:01:00' AND n1.dob from TDTT005timetz n1;
select n1.dob NOT BETWEEN n2.dob AND n1.dob from TDTT005timetz n1, TDTT005time n2;
select n1.dob NOT BETWEEN n2.dob AND n1.dob from TDTT005timetz n1, TDTT005timetz n2;
select n1.dob NOT BETWEEN NULL AND n1.dob from TDTT005timetz n1;
select time'01:01:00' NOT IN (select n1.dob) from TDTT005timetz n1;
select n1.dob NOT IN (select n2.dob) from TDTT005time n1, TDTT005timetz n2;
select n1.dob NOT IN (select time'01:01:00') from TDTT005timetz n1;
select n1.dob NOT IN (select n2.dob) from TDTT005timetz n1, TDTT005time n2;
select n1.dob NOT IN (n2.dob,n1.dob) from TDTT005timetz n1, TDTT005timetz n2;
select n1.dob NOT IN (select n2.dob) from TDTT005timetz n1, TDTT005timetz n2;
select n1.dob NOT IN (NULL,n1.dob) from TDTT005timetz n1;
select n1.dob NOT IN (select NULL) from TDTT005timetz n1;
select n1.dob - time'01:01:00' from TDTT005timetz n1;
select n1.dob - n2.dob from TDTT005timetz n1, TDTT005time n2;
select n1.dob - NULL from TDTT005timetz n1;
select time(zhorolog) with time zone ',40384,805003,-46776';

select COALESCE(time'01:01:00',time with time zone'01:01:00-05:00');
select GREATEST(time'01:01:00',time with time zone'01:01:00-05:00');
select LEAST(time'01:01:00',time with time zone'01:01:00-05:00');
select NULLIF(time'01:01:00',time with time zone'01:01:00-05:00');
select COALESCE(time with time zone'01:01:00-05:00',time'01:01:00');
select GREATEST(time with time zone'01:01:00-05:00',time'01:01:00');
select LEAST(time with time zone'01:01:00-05:00',time'01:01:00');
select NULLIF(time with time zone'01:01:00-05:00',time'01:01:00');
select COALESCE(time with time zone'01:01:00-05:00','01:01:01-05');
select GREATEST(time with time zone'01:01:00-05:00','01:01:01-05');
select LEAST(time with time zone'01:01:00-05:00','01:01:01-05');
select NULLIF(time with time zone'01:01:00-05:00','01:01:01-05');
select time'01:01:00' IN (time with time zone'01:01:00-05:00',time'01:01:00');
select time with time zone'01:01:00-05:00' IN (time'01:01:00',time with time zone'01:01:00-05:00');
select time with time zone'01:01:00-05:00' IN (n1.dob,time with time zone'01:01:00-05:00') from TDTT005time n1;
select time with time zone'01:01:00-05:00' IN ('01:01:01-05',time with time zone'01:01:00-05:00');
select n1.dob IN (time with time zone'01:01:00-05:00',n1.dob) from TDTT005time n1;
select time'01:01:00' NOT IN (time with time zone'01:01:00-05:00',time'01:01:00');
select time with time zone'01:01:00-05:00' NOT IN (time'01:01:00',time with time zone'01:01:00-05:00');
select time with time zone'01:01:00-05:00' NOT IN ('01:01:01-05',time with time zone'01:01:00-05:00');
select n1.dob NOT IN (time with time zone'01:01:00-05:00',n1.dob) from TDTT005time n1;
select time with time zone'01:01:00-05:00' NOT IN (n1.dob,time with time zone'01:01:00-05:00') from TDTT005time n1;
select time'01:01:00' IN (n1.dob,time'01:01:00') from TDTT005timetz n1;
select n1.dob IN (n2.dob,n1.dob) from TDTT005time n1, TDTT005timetz n2;
select n1.dob IN (time'01:01:00',n1.dob) from TDTT005timetz n1;
select n1.dob IN (n2.dob,n1.dob) from TDTT005timetz n1, TDTT005time n2;
select n1.dob IN ('01:01:01-05',n1.dob) from TDTT005timetz n1;
select time'01:01:00' NOT IN (n1.dob,time'01:01:00') from TDTT005timetz n1;
select n1.dob NOT IN (n2.dob,n1.dob) from TDTT005time n1, TDTT005timetz n2;
select n1.dob NOT IN (time'01:01:00',n1.dob) from TDTT005timetz n1;
select n1.dob NOT IN (n2.dob,n1.dob) from TDTT005timetz n1, TDTT005time n2;
select n1.dob NOT IN ('01:01:01-05',n1.dob) from TDTT005timetz n1;
drop table TDTT005date;
drop table TDTT005timetz;
drop table TDTT005timestamp;
drop table TDTT005timestamptz;
select time(zhorolog) with time zone ',25210,+78868,23370' + time '20:32:01';

-- Ensure order by optimization doesn't result in assert false for time with time zone
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/382
drop table if exists test;
create table test (foo time primary key);
insert into test values (time 'T01:01:01-05');
insert into test values (time '02:01:01-05');
select * from test order by foo; -- Used to cause an assert fail
drop table if exists test;
create table test(foo Time With Time Zone primary key) global "^testttz" readonly;
select * from test order by foo; -- Used to cause an assert fail

-- #1054
-- target: timetz
-- source: time,timetz
drop table if exists TDTT109;
create table TDTT109 (id int, dob time with time zone);
insert into TDTT109 values(1, time'01:01:01');
insert into TDTT109 values(2, time with time zone'01:01:01+06');
insert into TDTT109 values(3, time with time zone'01:01:01-05');
insert into TDTT109 values(4, time with time zone'01:01:01-04');
insert into TDTT109 values(5, '01:01:01');
insert into TDTT109 values(6, '01:01:01+06');
insert into TDTT109 values(7, '01:01:01-05');
insert into TDTT109 values(8, '01:01:01-04');
select * from TDTT109;
drop table TDTT109;

-- Values for the following queries change when daylight savings change
select time with time zone'01:01:00-05:00' || 'sample string';
select time with time zone'01:01:00-05:00' - time'01:01:00';
select time with time zone'01:01:00-05:00' - n1.dob from TDTT005time n1;
drop table TDTT005time;
