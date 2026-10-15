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

-- TOB24 : OCTO1160 : ORDER BY on a non-key column walks an AIM cross reference of that column

-- TIME WITH TIME ZONE and TIMESTAMP WITH TIME ZONE columns of a READWRITE table, as in TOB24_types.sql for the other types:
-- first as walked non-key columns, then as the primary key column after the walked column. The test runs with TZ=UTC, so
-- the values are shown with offset +00 and the output does not depend on daylight saving time.
create table TOB24tz (id integer primary key, ttz time with time zone, tstz timestamp with time zone);
insert into TOB24tz values (1, time with time zone '09:00:00+05', timestamp with time zone '2024-03-01 09:00:00+05'), (2, time with time zone '09:00:00-05', timestamp with time zone '2024-03-01 09:00:00-05');
insert into TOB24tz values (3, NULL, NULL), (4, time with time zone '23:59:59-03', timestamp with time zone '1999-12-31 23:59:59-03'), (5, time with time zone '10:00:00+00', timestamp with time zone '2024-03-01 10:00:00+00');
select * from TOB24tz order by ttz, id;
select * from TOB24tz order by ttz desc, id;
select * from TOB24tz order by tstz, id;
select * from TOB24tz order by tstz desc, id;

create table TOB24k_timetz (k time with time zone primary key, v varchar);
insert into TOB24k_timetz values (time with time zone '09:00:00+05', 'y'), (time with time zone '09:00:00-05', 'x'), (time with time zone '10:00:00+00', NULL), (time with time zone '23:59:59-03', 'x');
select * from TOB24k_timetz order by v, k;
select * from TOB24k_timetz order by v desc, k desc limit 3;

create table TOB24k_timestamptz (k timestamp with time zone primary key, v varchar);
insert into TOB24k_timestamptz values (timestamp with time zone '2024-03-01 09:00:00+05', 'y'), (timestamp with time zone '2024-03-01 09:00:00-05', 'x'), (timestamp with time zone '2024-03-01 10:00:00+00', NULL), (timestamp with time zone '1999-12-31 23:59:59-03', 'x');
select * from TOB24k_timestamptz order by v, k;
select * from TOB24k_timestamptz order by v desc, k desc limit 3;

-- The dates of a READONLY table are stored in the format of the table definition. Here that is text, read with the
-- MDY datestyle, so "03-01-2024" collates before "12-31-2023" in the global.
set datestyle = 'MDY';

-- A non-key date column of a READONLY table. Its AIM cross reference stores each date converted to UNIX time, so the
-- walk is in date order. ORDER BY optimization takes effect.
create table TOB24e (id integer primary key, d date) global "^TOB24e" readonly;
select * from TOB24e order by d, id;
select * from TOB24e order by d desc, id;

-- A non-key column of every data type in a READONLY table. Its AIM cross reference stores each value converted where
-- needed (dates and times to UNIX time, booleans from "t"/"true"/"f"/"0" to 1/0), so each walk is in value order.
-- ORDER BY optimization takes effect, except for the INTEGER and NUMERIC columns (see 2) below).
create table TOB24h (id integer primary key, b boolean, i integer, n numeric, s varchar, d date, t time, ttz time with time zone, ts timestamp, tstz timestamp with time zone) global "^TOB24h" readonly;
select id, b from TOB24h order by b, id;
select id, b from TOB24h order by b desc, id;
select id, s from TOB24h order by s, id;
select id, s from TOB24h order by s desc, id;
select id, d from TOB24h order by d, id;
select id, d from TOB24h order by d desc, id;
select id, t from TOB24h order by t, id;
select id, t from TOB24h order by t desc, id;
select id, ttz from TOB24h order by ttz, id;
select id, ttz from TOB24h order by ttz desc, id;
select id, ts from TOB24h order by ts, id;
select id, ts from TOB24h order by ts desc, id;
select id, tstz from TOB24h order by tstz, id;
select id, tstz from TOB24h order by tstz desc, id;

-- ORDER BY optimization does not take effect
-- 1) The second ORDER BY column is a date key column of a READONLY table. Its FOR loop walks the subscripts as stored, in
--    the order "01-15-2024", "03-01-2024", "12-31-2023", which is not date order.
create table TOB24c (d date primary key, n integer) global "^TOB24c" readonly;
select * from TOB24c order by n, d;
-- The same for the other date/time types as the key column after the walked column v (MDY text, "10:00:00" before "9:00:00")
create table TOB24r_time (k time primary key, v varchar) global "^TOB24rtime" readonly;
select * from TOB24r_time order by v, k;
create table TOB24r_timetz (k time with time zone primary key, v varchar) global "^TOB24rtimetz" readonly;
select * from TOB24r_timetz order by v, k;
create table TOB24r_timestamp (k timestamp primary key, v varchar) global "^TOB24rtimestamp" readonly;
select * from TOB24r_timestamp order by v, k;
create table TOB24r_timestamptz (k timestamp with time zone primary key, v varchar) global "^TOB24rtimestamptz" readonly;
select * from TOB24r_timestamptz order by v, k;

-- 2) An INTEGER or NUMERIC column of a READONLY table. Its AIM cross reference stores the values as they are in the global,
--    and a value that is not a canonical number ("-0.25") is a string subscript there, which collates after all numbers
--    (YDBOcto#1168). Also with a WHERE range on the column, which walks the same cross reference.
select id, i from TOB24h order by i, id;
select id, i from TOB24h order by i desc, id;
select id, n from TOB24h order by n, id;
select id, n from TOB24h order by n desc, id;
select id, i from TOB24h where i > 0 order by i desc, id;

-- 3) The ORDER BY column is in a local variable (pg_settings), so it has no AIM cross reference.
--    LIMIT 0 keeps the output independent of the settings of the environment.
select name from pg_catalog.pg_settings order by setting limit 0;

-- 4) The ORDER BY column is an EXTRACT column, which cannot be cross referenced
create table TOB24d (id integer primary key, last varchar extract "$PIECE($GET(^names(keys(""id""))),""|"",2)") global "^names" readonly;
select * from TOB24d order by last, id;
