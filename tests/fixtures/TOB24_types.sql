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

-- INTEGER, NUMERIC, BOOLEAN, DATE and VARCHAR non-key columns with NULL values and duplicate values
drop table if exists TOB24a;
create table TOB24a (id integer primary key, i integer, n numeric, b boolean, d date, s varchar);
insert into TOB24a values (1, 10, 1.5, true, date '2024-03-01', '10');
insert into TOB24a values (2, -3, -0.25, false, date '1999-12-31', '9');
insert into TOB24a values (3, NULL, NULL, NULL, NULL, NULL);
insert into TOB24a values (4, 10, 100, true, date '2024-03-01', 'A1');
insert into TOB24a values (5, 0, 0, false, date '2000-01-01', 'a');
insert into TOB24a values (6, NULL, 2, NULL, date '2030-06-15', 'b');
insert into TOB24a values (7, 99, NULL, true, NULL, '-1');

select * from TOB24a order by i, id;
select * from TOB24a order by i desc, id;
select * from TOB24a order by n, id;
select * from TOB24a order by n desc, id desc;
select * from TOB24a order by b, id;
select * from TOB24a order by b desc, id;
select * from TOB24a order by d, id;
select * from TOB24a order by d desc, id;
select * from TOB24a order by s, id;
select * from TOB24a order by s desc, id;
select id, i from TOB24a order by i desc, id limit 3;
select id, s from TOB24a order by s, id limit 3;
select * from TOB24a where i > 0 order by i desc, id;
select * from TOB24a where s > '1' order by s desc, id;
select * from TOB24a where n <= 1.5 order by n desc, id;

-- VARCHAR column with Japanese and Chinese values (3 bytes per character in UTF-8) and a LIKE prefix in both directions.
-- "み" is E3 81 BF in UTF-8, so the descending walk for 'み%' starts at E3 81 C0 (not valid UTF-8). "む" (E3 82 80) is the
-- next character and must not be returned. ILIKE ends the prefix at the first non-ASCII byte, so '東%' walks every value.
drop table if exists TOB24f;
create table TOB24f (id integer primary key, v varchar);
insert into TOB24f values (1, '東京'), (2, '東京都'), (3, '東北'), (4, '東'), (5, '京都'), (6, '北京'), (7, '日本');
insert into TOB24f values (8, '日本語'), (9, NULL), (10, 'みかん'), (11, 'みそ'), (12, 'む'), (13, 'むら'), (14, '東京');
select * from TOB24f where v like '東%' order by v desc, id;
select * from TOB24f where v like '東京%' order by v desc, id desc;
select * from TOB24f where v like '日本%' order by v, id;
select * from TOB24f where v like 'み%' order by v desc, id;
select * from TOB24f where v like 'み%' order by v, id;
select * from TOB24f where v ilike '東%' order by v desc, id;
select * from TOB24f order by v desc, id limit 4;

-- ORDER BY optimization does not take effect: the second ORDER BY column is a VARCHAR key column, whose M collation
-- (canonical numbers first) is not the ORDER BY order
drop table if exists TOB24b;
create table TOB24b (code varchar primary key, n integer);
insert into TOB24b values ('9', 1);
insert into TOB24b values ('10', 1);
insert into TOB24b values ('A1', 1);
insert into TOB24b values ('B2', 0);
select * from TOB24b order by n, code;

-- TIME and TIMESTAMP non-key columns of a READWRITE table, with NULL values and duplicate values (TIME WITH TIME ZONE and
-- TIMESTAMP WITH TIME ZONE are in TOB24_octoonly.sql, as Octo shows them in the session time zone and Postgres with the
-- offset they were entered with)
drop table if exists TOB24g;
create table TOB24g (id integer primary key, t time, ts timestamp);
insert into TOB24g values (1, time '10:00:00', timestamp '2024-03-01 10:00:00'), (2, time '09:00:00', timestamp '1999-12-31 23:59:59');
insert into TOB24g values (3, NULL, NULL), (4, time '23:59:59.5', timestamp '2024-03-01 09:00:00'), (5, time '09:00:00', timestamp '2024-03-01 10:00:00');
insert into TOB24g values (6, time '00:00:00', timestamp '2000-01-01 00:00:00');
select * from TOB24g order by t, id;
select * from TOB24g order by t desc, id;
select * from TOB24g order by ts, id;
select * from TOB24g order by ts desc, id desc;
select id, t from TOB24g order by t desc, id limit 2;

-- Each data type as the primary key column after the walked column: within one value of v, the rows come out in the
-- order of k. ORDER BY optimization takes effect for every type except VARCHAR, whose M collation is not the ORDER BY order.
drop table if exists TOB24k_boolean;
create table TOB24k_boolean (k boolean primary key, v varchar);
insert into TOB24k_boolean values (false, 'y'), (true, 'y');
select * from TOB24k_boolean order by v, k;
select * from TOB24k_boolean order by v desc, k desc limit 3;

drop table if exists TOB24k_integer;
create table TOB24k_integer (k integer primary key, v varchar);
insert into TOB24k_integer values (-3, 'y'), (0, 'x'), (2, NULL), (10, 'x');
select * from TOB24k_integer order by v, k;
select * from TOB24k_integer order by v desc, k desc limit 3;

drop table if exists TOB24k_numeric;
create table TOB24k_numeric (k numeric primary key, v varchar);
insert into TOB24k_numeric values (-0.25, 'y'), (1.5, 'x'), (2.75, NULL), (10, 'x');
select * from TOB24k_numeric order by v, k;
select * from TOB24k_numeric order by v desc, k desc limit 3;

drop table if exists TOB24k_varchar;
create table TOB24k_varchar (k varchar primary key, v varchar);
insert into TOB24k_varchar values ('9', 'y'), ('10', 'x'), ('A1', NULL), ('b', 'x');
select * from TOB24k_varchar order by v, k;
select * from TOB24k_varchar order by v desc, k desc limit 3;

drop table if exists TOB24k_date;
create table TOB24k_date (k date primary key, v varchar);
insert into TOB24k_date values (date '1999-12-31', 'y'), (date '2000-01-01', 'x'), (date '2024-03-01', NULL), (date '2030-06-15', 'x');
select * from TOB24k_date order by v, k;
select * from TOB24k_date order by v desc, k desc limit 3;

drop table if exists TOB24k_time;
create table TOB24k_time (k time primary key, v varchar);
insert into TOB24k_time values (time '00:00:00', 'y'), (time '09:00:00', 'x'), (time '10:00:00', NULL), (time '23:59:59.5', 'x');
select * from TOB24k_time order by v, k;
select * from TOB24k_time order by v desc, k desc limit 3;

drop table if exists TOB24k_timestamp;
create table TOB24k_timestamp (k timestamp primary key, v varchar);
insert into TOB24k_timestamp values (timestamp '1999-12-31 23:59:59', 'y'), (timestamp '2000-01-01 00:00:00', 'x'), (timestamp '2024-03-01 09:00:00', NULL), (timestamp '2024-03-01 10:00:00.5', 'x');
select * from TOB24k_timestamp order by v, k;
select * from TOB24k_timestamp order by v desc, k desc limit 3;
