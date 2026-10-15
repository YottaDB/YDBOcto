#################################################################
#								#
# Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TOB23 : OCTO1161 : ORDER BY on a key column is optimized when the key columns before it are fixed to one value

-- A READONLY table whose leading key column is VARCHAR (the example in YDBOcto#1161).
create table TOB23 (test_code varchar, isn integer, result varchar, primary key (test_code, isn)) global "^TOB23" readonly;

-- Leading VARCHAR key column fixed with "=". ORDER BY optimization takes effect.
select * from TOB23 where test_code = 'PSA' order by isn desc limit 1;
select * from TOB23 where test_code = 'PSA' order by isn;
-- The fixed VARCHAR key column is itself an ORDER BY column. ORDER BY optimization takes effect.
select * from TOB23 where test_code = 'PSA' order by test_code desc, isn desc limit 2;

-- ORDER BY optimization does not take effect
-- 1) Leading key column is not fixed
select * from TOB23 order by isn desc limit 1;
-- 2) Leading key column is fixed to a LIST of values
select * from TOB23 where test_code in ('PSA', 'A1C') order by isn desc limit 1;
-- 3) ORDER BY on the VARCHAR key column that is not fixed
select * from TOB23 where isn = 2 order by test_code;
-- 4) The leading key column is fixed with "=", but the next key column is VARCHAR. Its FOR loop walks the subscripts in
--    M collation ("-1", "1.5", "9", "10", "A1", "b"), which is not the ORDER BY order, so the rows are still sorted.
create table TOB23V (test_code varchar, isn varchar, result varchar, primary key (test_code, isn)) global "^TOB23V" readonly;
select * from TOB23V where test_code = 'PSA' order by isn desc limit 1;
select * from TOB23V where test_code = 'PSA' order by isn;
select * from TOB23V where test_code = 'PSA' order by test_code desc, isn desc;

-- ORDER BY optimization takes effect
-- 5) TIME WITH TIME ZONE and TIMESTAMP WITH TIME ZONE key columns of a READWRITE table, as in TOB23_types.sql for the other
--    types. The test runs with TZ=UTC, so the values are shown with offset +00 and the output does not depend on daylight
--    saving time.
create table TOB23_timetz (k1 time with time zone, k2 time with time zone, primary key (k1, k2));
insert into TOB23_timetz values (time with time zone '01:02:03+05', time with time zone '09:00:00+05'), (time with time zone '01:02:03+05', time with time zone '09:00:00-05'), (time with time zone '01:02:03+05', time with time zone '10:00:00+00'), (time with time zone '01:02:03+05', time with time zone '23:59:59-03');
insert into TOB23_timetz values (time with time zone '23:59:59-05', time with time zone '09:00:00+05'), (time with time zone '23:59:59-05', time with time zone '09:00:00-05'), (time with time zone '23:59:59-05', time with time zone '10:00:00+00'), (time with time zone '23:59:59-05', time with time zone '23:59:59-03');
select * from TOB23_timetz where k1 = time with time zone '01:02:03+05' order by k2;
select * from TOB23_timetz where k1 = time with time zone '01:02:03+05' order by k2 desc limit 2;
select * from TOB23_timetz where k1 = time with time zone '23:59:59-05' order by k1 desc, k2;
create table TOB23_timestamptz (k1 timestamp with time zone, k2 timestamp with time zone, primary key (k1, k2));
insert into TOB23_timestamptz values (timestamp with time zone '2024-03-01 01:02:03+05', timestamp with time zone '2024-03-01 09:00:00+05'), (timestamp with time zone '2024-03-01 01:02:03+05', timestamp with time zone '2024-03-01 09:00:00-05'), (timestamp with time zone '2024-03-01 01:02:03+05', timestamp with time zone '2024-03-01 10:00:00+00'), (timestamp with time zone '2024-03-01 01:02:03+05', timestamp with time zone '1999-12-31 23:59:59-03');
insert into TOB23_timestamptz values (timestamp with time zone '1999-12-31 23:59:59-05', timestamp with time zone '2024-03-01 09:00:00+05'), (timestamp with time zone '1999-12-31 23:59:59-05', timestamp with time zone '2024-03-01 09:00:00-05'), (timestamp with time zone '1999-12-31 23:59:59-05', timestamp with time zone '2024-03-01 10:00:00+00'), (timestamp with time zone '1999-12-31 23:59:59-05', timestamp with time zone '1999-12-31 23:59:59-03');
select * from TOB23_timestamptz where k1 = timestamp with time zone '2024-03-01 01:02:03+05' order by k2;
select * from TOB23_timestamptz where k1 = timestamp with time zone '2024-03-01 01:02:03+05' order by k2 desc limit 2;
select * from TOB23_timestamptz where k1 = timestamp with time zone '1999-12-31 23:59:59-05' order by k1 desc, k2;

-- ORDER BY optimization does not take effect
-- 6) Date/time key columns of a READONLY table. The values are stored as MDY text, so the FOR loop of k2 walks them in
--    string order ("03-01-2024" before "12-31-2023", "10:00:00" before "9:00:00"), which is not the ORDER BY order. The
--    rows are still sorted. The fixed key k1 is skipped as for the other types.
set datestyle = 'MDY';
create table TOB23R_date (k1 date, k2 date, primary key (k1, k2)) global "^TOB23Rdate" readonly;
select * from TOB23R_date where k1 = date '03-01-2024' order by k2;
select * from TOB23R_date where k1 = date '03-01-2024' order by k2 desc limit 2;
create table TOB23R_time (k1 time, k2 time, primary key (k1, k2)) global "^TOB23Rtime" readonly;
select * from TOB23R_time where k1 = time '01:02:03' order by k2;
select * from TOB23R_time where k1 = time '01:02:03' order by k2 desc limit 2;
create table TOB23R_timetz (k1 time with time zone, k2 time with time zone, primary key (k1, k2)) global "^TOB23Rtimetz" readonly;
select * from TOB23R_timetz where k1 = time with time zone '01:02:03+00' order by k2;
select * from TOB23R_timetz where k1 = time with time zone '01:02:03+00' order by k2 desc limit 2;
create table TOB23R_timestamp (k1 timestamp, k2 timestamp, primary key (k1, k2)) global "^TOB23Rtimestamp" readonly;
select * from TOB23R_timestamp where k1 = timestamp '03-01-2024 01:02:03' order by k2;
select * from TOB23R_timestamp where k1 = timestamp '03-01-2024 01:02:03' order by k2 desc limit 2;
create table TOB23R_timestamptz (k1 timestamp with time zone, k2 timestamp with time zone, primary key (k1, k2)) global "^TOB23Rtimestamptz" readonly;
select * from TOB23R_timestamptz where k1 = timestamp with time zone '03-01-2024 01:02:03+00' order by k2;
select * from TOB23R_timestamptz where k1 = timestamp with time zone '03-01-2024 01:02:03+00' order by k2 desc limit 2;
