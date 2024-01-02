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
drop table if exists test1;
create table test1 (id int, dob date);
insert into test1 values(1, date'2023-01-01');
select * from test1;

-- Timestamp
-- YYYY-MM-DD HH:MM:SS
-- without timezone
drop table if exists test2;
create table test2 (id int, dob timestamp);
insert into test2 values(1,timestamp'2023-01-01 11:00:00');
insert into test2 values(2,timestamp without time zone'2023-01-02 11:22:00');
select * from test2;

-- Time
-- HH:MM:SS
-- without timezone
drop table if exists test3;
create table test3 (id int, tob time);
insert into test3 values(1, time '11:00:00');
insert into test3 values(2, time without time zone '15:00:00');
select * from test3;

-- Timestamp with time zone
-- YYYY-MM-DD HH:MM:SS.uuuuuu-/+HHMM
-- with timezone
drop table if exists test2wt;
create table test2wt (id int, dob timestamp with time zone);
insert into test2wt values(1,timestamp with time zone'2023-01-01 11:00:00-08:30');
insert into test2wt values(2,timestamp with time zone'2023-01-01 11:00:00-08:00');
select * from test2wt;
