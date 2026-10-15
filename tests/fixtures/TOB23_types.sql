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

-- One READWRITE table per data type, with PRIMARY KEY (k1, k2) of that type (TIME WITH TIME ZONE and TIMESTAMP WITH TIME
-- ZONE are in TOB23_octoonly.sql, as Octo shows them in the session time zone and Postgres with the offset they were
-- entered with). For each type:
--   "WHERE k1 = ... ORDER BY k2" and "ORDER BY k2 DESC LIMIT 2": the fixed key k1 is skipped, and the FOR loop of k2 gives
--   the output order. "ORDER BY k1 DESC, k2": the ORDER BY column k1 is fixed, so it is consumed whatever its type.
-- ORDER BY optimization takes effect for every type except VARCHAR, whose M collation (canonical numbers first) is not the
-- ORDER BY order. Both k1 values have all k2 values, so that a sort over all rows would give a different answer.

drop table if exists TOB23_boolean;
create table TOB23_boolean (k1 boolean, k2 boolean, primary key (k1, k2));
insert into TOB23_boolean values (true, false), (true, true), (false, false), (false, true);
select * from TOB23_boolean where k1 = true order by k2;
select * from TOB23_boolean where k1 = true order by k2 desc limit 2;
select * from TOB23_boolean where k1 = false order by k1 desc, k2;

drop table if exists TOB23_integer;
create table TOB23_integer (k1 integer, k2 integer, primary key (k1, k2));
insert into TOB23_integer values (1, -3), (1, 0), (1, 2), (1, 10), (2, -3), (2, 0), (2, 2), (2, 10);
select * from TOB23_integer where k1 = 1 order by k2;
select * from TOB23_integer where k1 = 1 order by k2 desc limit 2;
select * from TOB23_integer where k1 = 2 order by k1 desc, k2;

drop table if exists TOB23_numeric;
create table TOB23_numeric (k1 numeric, k2 numeric, primary key (k1, k2));
insert into TOB23_numeric values (1.5, -0.25), (1.5, 1.5), (1.5, 2.75), (1.5, 10), (2, -0.25), (2, 1.5), (2, 2.75), (2, 10);
select * from TOB23_numeric where k1 = 1.5 order by k2;
select * from TOB23_numeric where k1 = 1.5 order by k2 desc limit 2;
select * from TOB23_numeric where k1 = 2 order by k1 desc, k2;

drop table if exists TOB23_varchar;
create table TOB23_varchar (k1 varchar, k2 varchar, primary key (k1, k2));
insert into TOB23_varchar values ('a', '9'), ('a', '10'), ('a', 'A1'), ('a', 'b'), ('b', '9'), ('b', '10'), ('b', 'A1'), ('b', 'b');
select * from TOB23_varchar where k1 = 'a' order by k2;
select * from TOB23_varchar where k1 = 'a' order by k2 desc limit 2;
select * from TOB23_varchar where k1 = 'b' order by k1 desc, k2;

drop table if exists TOB23_date;
create table TOB23_date (k1 date, k2 date, primary key (k1, k2));
insert into TOB23_date values (date '2024-03-01', date '1999-12-31'), (date '2024-03-01', date '2000-01-01'), (date '2024-03-01', date '2024-03-01'), (date '2024-03-01', date '2030-06-15'), (date '1999-12-31', date '1999-12-31'), (date '1999-12-31', date '2000-01-01'), (date '1999-12-31', date '2024-03-01'), (date '1999-12-31', date '2030-06-15');
select * from TOB23_date where k1 = date '2024-03-01' order by k2;
select * from TOB23_date where k1 = date '2024-03-01' order by k2 desc limit 2;
select * from TOB23_date where k1 = date '1999-12-31' order by k1 desc, k2;

drop table if exists TOB23_time;
create table TOB23_time (k1 time, k2 time, primary key (k1, k2));
insert into TOB23_time values (time '01:02:03', time '00:00:00'), (time '01:02:03', time '09:00:00'), (time '01:02:03', time '10:00:00'), (time '01:02:03', time '23:59:59.5'), (time '23:59:59', time '00:00:00'), (time '23:59:59', time '09:00:00'), (time '23:59:59', time '10:00:00'), (time '23:59:59', time '23:59:59.5');
select * from TOB23_time where k1 = time '01:02:03' order by k2;
select * from TOB23_time where k1 = time '01:02:03' order by k2 desc limit 2;
select * from TOB23_time where k1 = time '23:59:59' order by k1 desc, k2;

drop table if exists TOB23_timestamp;
create table TOB23_timestamp (k1 timestamp, k2 timestamp, primary key (k1, k2));
insert into TOB23_timestamp values (timestamp '2024-03-01 01:02:03', timestamp '1999-12-31 23:59:59'), (timestamp '2024-03-01 01:02:03', timestamp '2000-01-01 00:00:00'), (timestamp '2024-03-01 01:02:03', timestamp '2024-03-01 09:00:00'), (timestamp '2024-03-01 01:02:03', timestamp '2024-03-01 10:00:00.5'), (timestamp '1999-12-31 23:59:59', timestamp '1999-12-31 23:59:59'), (timestamp '1999-12-31 23:59:59', timestamp '2000-01-01 00:00:00'), (timestamp '1999-12-31 23:59:59', timestamp '2024-03-01 09:00:00'), (timestamp '1999-12-31 23:59:59', timestamp '2024-03-01 10:00:00.5');
select * from TOB23_timestamp where k1 = timestamp '2024-03-01 01:02:03' order by k2;
select * from TOB23_timestamp where k1 = timestamp '2024-03-01 01:02:03' order by k2 desc limit 2;
select * from TOB23_timestamp where k1 = timestamp '1999-12-31 23:59:59' order by k1 desc, k2;
