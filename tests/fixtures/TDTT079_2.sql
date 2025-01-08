#################################################################
#								#
# Copyright (c) 2025 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- Output of following queries differ in Octo and Postgres
-- true in Postgres false in Octo
select timestamp with time zone'2024-11-03 01:49:19-05' = timestamp with time zone'2024-11-03 01:49:19';
-- false in Postgres true in Octo
select timestamp with time zone'2024-11-03 01:49:19-04' = timestamp with time zone'2024-11-03 01:49:19';
-- `2024-11-03 01:49:19-05` in Postgres `2024-11-03 01:49:19-04` in Octo
select timestamp with time zone'2024-11-03 01:49:19';

-- Similarly
select timestamp with time zone '2194-11-02 01:49:19-05' = timestamp(horolog) with time zone '129237,6559';
select timestamp with time zone '2194-11-02 01:49:19-04' = timestamp(horolog) with time zone '129237,6559';
select timestamp with time zone '2194-11-02 01:49:19-05' = timestamp with time zone '2194-11-02 01:49:19';
select timestamp with time zone '2194-11-02 01:49:19-04' = timestamp with time zone '2194-11-02 01:49:19';

-- Postgres returns t for this Octo returns t
select timestamp with time zone '2194-11-02 01:49:19-05' = timestamp(horolog)'129237,6559';
-- Postgres returns f for this Octo returns t
select timestamp with time zone '2194-11-02 01:49:19-04' = timestamp(horolog)'129237,6559';
-- Postgres returns t for this Octo returns t
select timestamp with time zone '2024-11-03 01:49:19-05'=timestamp'2024-11-03 01:49:19';
-- Postgres returns f for this Octo returns t
select timestamp with time zone '2024-11-03 01:49:19-04'=timestamp'2024-11-03 01:49:19';
-- At 02 clock is forwarded to 03 and timezone becomes -04 so following doesn't make sense but Postgres
-- returns t for it and Octo returns f
select timestamp with time zone '2024-03-10 02:49:19-05'=timestamp'2024-03-10 02:49:19';

-- Select query below failed in TDTT088 and a similar one failed in TDTT087 because of the reason explained
-- in second commit message of https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1620
create table timestamptz_tbl (order_id integer primary key, order_timestamptz timestamp with time zone) GLOBAL "^timestamptztbl" READWRITE;
create table timestamptz_horolog_tbl (order_id integer primary key, order_timestamptz timestamp(horolog) with time zone) GLOBAL "^timestamptzhorologtbl" READWRITE;
insert into timestamptz_horolog_tbl  values (179, timestamp(horolog) with time zone '129237,6559');
insert into timestamptz_tbl          values (177, timestamp with time zone '2194-11-02 01:49:19-05');
select count(*) >= 1 from timestamptz_tbl t1 inner join timestamptz_horolog_tbl t2 on t1.order_timestamptz = t2.order_timestamptz;
select * from timestamptz_tbl t1 , timestamptz_horolog_tbl t2;
-- Octo considers `timestamp'2194-11-02 01:49:19'` to be timestamp with time zone'2194-11-02 01:49:19-04'
-- Postgres considers `timestamp'2194-11-02 01:49:19'` to be timestamp with time zone'2194-11-02 01:49:19-05'
-- This leads to difference in output between Octo and Postgres for the following queries
-- Postgres output 2194-11-02 01:49:19-05
select greatest(timestamp with time zone'2194-11-02 01:49:00-04',timestamp'2194-11-02 01:49:19');
select greatest(timestamp'2194-11-02 01:49:19',timestamp with time zone'2194-11-02 01:49:00-04');
-- Postgres output 2194-11-02 01:49:19-05
select greatest(timestamp with time zone'2194-11-02 01:49:00-05',timestamp'2194-11-02 01:49:19');
select greatest(timestamp'2194-11-02 01:49:19',timestamp with time zone'2194-11-02 01:49:00-05');
-- Postgres output 2194-11-02 01:49:00-05
select least(timestamp with time zone'2194-11-02 01:49:00-05',timestamp'2194-11-02 01:49:19');
select least(timestamp'2194-11-02 01:49:19',timestamp with time zone'2194-11-02 01:49:00-05');
