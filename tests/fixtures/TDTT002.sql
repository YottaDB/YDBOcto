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

-- Date format
-- YYYY-MM-DD
drop table if exists tdtt0021;
create table tdtt0021 (id int, dob date);
insert into tdtt0021 values(1, date'2023-01-01');
select * from tdtt0021;
drop table tdtt0021;

-- Timestamp
-- YYYY-MM-DD HH:MM:SS
-- without timezone
drop table if exists tdtt0022;
create table tdtt0022 (id int, dob timestamp);
insert into tdtt0022 values(1,timestamp'2023-01-01 11:00:00');
insert into tdtt0022 values(2,timestamp without time zone'2023-01-02 11:22:00');
select * from tdtt0022;
drop table tdtt0022;

-- Time
-- HH:MM:SS
-- without timezone
drop table if exists tdtt0023;
create table tdtt0023 (id int, tob time);
insert into tdtt0023 values(1, time '11:00:00');
insert into tdtt0023 values(2, time without time zone '15:00:00');
select * from tdtt0023;
drop table tdtt0023;

-- Timestamp with time zone
-- YYYY-MM-DD HH:MM:SS.uuuuuu-/+HHMM
-- with timezone
drop table if exists tdtt0022wt;
create table tdtt0022wt (id int, dob timestamp with time zone);
insert into tdtt0022wt values(1,timestamp with time zone'2023-01-01 11:00:00-08:30');
insert into tdtt0022wt values(2,timestamp with time zone'2023-01-01 11:00:00-08:00');
select * from tdtt0022wt;
drop table tdtt0022wt;

-- ISO 8601 format
-- Timestamp
-- YYYY-MM-DDTHH:MM:SS.uuuuuu
-- without timezone
drop table if exists tdtt002;
create table tdtt002 (id int, dob timestamp);
insert into tdtt002 values(1,timestamp'2023-01-01T11:00:00.111111');
insert into tdtt002 values(2,timestamp'2023-01-01T11:00:00.111111111111');
insert into tdtt002 values(3,timestamp without time zone'2023-01-02T11:22:00');
insert into tdtt002 values(4,timestamp'2023-01-01 11:00:00.111111');
select * from tdtt002;
drop table tdtt002;

-- Time
-- THH:MM:SS
-- without timezone
drop table if exists tdtt002;
create table tdtt002 (id int, tob time);
insert into tdtt002 values(1, time 'T11:00:00');
insert into tdtt002 values(2, time without time zone 'T15:00:00');
insert into tdtt002 values(3, time '11:00:00');
select * from tdtt002;
drop table tdtt002;

-- Timestamp with time zone
-- YYYY-MM-DDTHH:MM:SS.uuuuuu-/+HHMM
-- with timezone
drop table if exists tdtt0022wt;
create table tdtt0022wt (id int, dob timestamp with time zone);
insert into tdtt0022wt values(1,timestamp with time zone'2023-01-01T11:00:00.111111-08:30');
insert into tdtt0022wt values(2,timestamp with time zone'2023-01-01T11:00:00.1111111111111-08:00');
insert into tdtt0022wt values(3,timestamp with time zone'2023-01-01 11:00:00.111111-08:30');
select * from tdtt0022wt;
drop table tdtt0022wt;
