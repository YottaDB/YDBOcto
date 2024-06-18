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

-- fileman (YYYMMDD.HHMMSS)
-- Following are the values used in this test
-- Date to fileman date
-- 2023-10-17 -> 66764 -> 3231017
-- 2023-10-16 -> 66763 -> 3231016
-- 2023-10-18 -> 66765 -> 3231018
-- 2023-10-19 -> 66766 -> 3231019
-- 2023-10-20 -> 66767 -> 3231020
-- 2024-10-17 -> 67130 -> 3241017

-- Time to fileman time
-- 01:01:01 -> 3661 -> 010101
-- 02:01:01 -> 7261 -> 020101
-- 03:01:01 -> 10861 -> 030101
-- 03:00:00 -> 10800 -> 030000
-- 04:00:00 -> 14400 -> 040000
-- 23:59:59 -> 86399 -> 235959
-- 23:00:01 -> 82801 -> 230001
-- 22:00:00 -> 79200 -> 220000
-- 01:02:01 -> 3721 -> 010201
-- 23:00:00 -> 82800 -> 230000

-- Timestamp to fileman timestamp
-- 2023-10-17 01:01:01 -> 66764,3661 -> 3231017.010101
-- 2023-10-16 02:01:01 -> 66763,7261 -> 3231016.020101
-- 2023-10-18 01:01:01 -> 66765,3661 -> 3231018.010101
-- 2023-10-18 03:01:01 -> 66765,10861 -> 3231018.030101
-- 2023-10-19 03:00:00 -> 66766,10800 -> 3231019.030000
-- 2023-10-20 04:00:00 -> 66767,14400 -> 3231020.040000
-- 2024-10-17 23:59:59 -> 67130,86399 -> 3241017.235959
-- 2023-10-16 01:02:01 -> 66763,3721 -> 3231016.010201
-- 2023-10-18 23:00:01 -> 66765,82801 -> 3231018.230001
-- 2023-10-17 22:00:00 -> 66764,79200 -> 3231017.220000
-- 2023-10-18 23:00:00 -> 66765,82800 -> 3231018.230000
drop table if exists testd;
create table testd (id integer, dob date);
insert into testd values(1,date(fileman)'3231017');
insert into testd values(2,date(fileman)'3231016');
select * from testd;

--drop table if exists testt;
--create table testt (id integer, tob time);
--insert into testt values(1,timestamp(fileman)'010101');
--insert into testt values(2,timestamp(fileman)'020101');
--select * from testt;

drop table if exists testts;
create table testts(id integer, dob timestamp);
insert into testts values(1, timestamp(fileman)'3231017.010101');
insert into testts values(2, timestamp(fileman)'3231016.020101');
select * from testts;

-- Basic select
select * from testd;
--select * from testt;
select * from testts;

select dob,firstname from testd,names;
--select tob,firstname from testt,names;
select dob,firstname from testts,names;

select date(fileman)'3231017',firstname from names limit 1;
--select timestamp(fileman)'010101',firstname from names limit 1;
select timestamp(fileman)'3231016.020101',firstname from names limit 1;

select dob from testd group by dob;
select dob from testd order by dob;
select dob from testd limit 1;
select distinct dob from testd;
select count(dob) from testd;
select count(distinct dob) from testd;
select distinct dob from testd group by dob order by dob limit 1;
select distinct date(fileman)'3231017' from testd group by 1 order by 1 limit 1;
-- Values
select column1 from (values(date(fileman)'3231017'));
values(date(fileman)'3231017');
-- set operation
select * from (select date(fileman)'3231017' union select date(fileman)'3231016');
select * from (select dob from testd union select dob from testd);
select date(fileman)'3231017' union select date(fileman)'3231016';
select date(fileman)'3231017' union all select date(fileman)'3231016' union all select date(fileman)'3231018';
(select date(fileman)'3231017' union all select date(fileman)'3231017') union all (select date(fileman)'3231016' union all select date(fileman)'3231016');
select date(fileman)'3231017' union select date(fileman)'3231016' union select date(fileman)'3231018';
(select date(fileman)'3231017' union select date(fileman)'3231017') union (select date(fileman)'3231016' union select date(fileman)'3231016');
(select date(fileman)'3231017' intersect select date(fileman)'3231017') intersect (select date(fileman)'3231017' union select date(fileman)'3231016');
(select date(fileman)'3231017' union select date(fileman)'3231019' union select date(fileman)'3231020') except (select date(fileman)'3231017' union select date(fileman)'3231016');

-- sub query
select (select dob from testd limit 1);
-- table.*
select testd.* from testd;

--select tob from testt group by tob;
--select tob from testt order by tob;
--select tob from testt limit 1;
--select distinct tob from testt;
--select count(tob) from testt;
--select count(distinct tob) from testt;
--select distinct tob from testt group by tob order by tob limit 1;
--select distinct timestamp(fileman)'010101' from testt group by 1 order by 1 limit 1;
-- Values
--select column1 from (values(timestamp(fileman)'010101'));
--values(timestamp(fileman)'010101');
-- set operation
--select * from (select timestamp(fileman)'010101' union select timestamp(fileman)'020101');
--select * from (select tob from testt union select tob from testt);
--select timestamp(fileman)'010101' union select timestamp(fileman)'020101';
--select timestamp(fileman)'010101' union all select timestamp(fileman)'020101' union all select timestamp(fileman)'010101';
--(select timestamp(fileman)'010101' union all select timestamp(fileman)'010101') union all (select timestamp(fileman)'020101' union all select timestamp(fileman)'020101');
--select timestamp(fileman)'010101' union select timestamp(fileman)'020101' union select timestamp(fileman)'030101';
--(select timestamp(fileman)'010101' union select timestamp(fileman)'010101') union (select timestamp(fileman)'020101' union select timestamp(fileman)'020101');
--(select timestamp(fileman)'010101' intersect select timestamp(fileman)'010101') intersect (select timestamp(fileman)'010101' union select timestamp(fileman)'020101');
--(select timestamp(fileman)'010101' union select timestamp(fileman)'030000' union select timestamp(fileman)'040000') except (select timestamp(fileman)'010101' union select timestamp(fileman)'020101');
-- sub query
--select (select tob from testt limit 1);
-- table.*
--select testt.* from testt;

select dob from testts group by dob;
select dob from testts order by dob;
select dob from testts limit 1;
select distinct dob from testts;
select count(dob) from testts;
select count(distinct dob) from testts;
select distinct dob from testts group by dob order by dob limit 1;
select distinct timestamp(fileman)'3231017.010101' from testts group by 1 order by 1 limit 1;
-- Values
select column1 from (values(timestamp(fileman)'3231017.010101'));
values(timestamp(fileman)'3231017.010101');
-- set operation
select * from (select timestamp(fileman)'3231017.010101' union select timestamp(fileman)'3231016.020101');
select * from (select dob from testts union select dob from testts);
select timestamp(fileman)'3231017.010101' union select timestamp(fileman)'3231016.020101';
select timestamp(fileman)'3231017.010101' union all select timestamp(fileman)'3231016.020101' union all select timestamp(fileman)'3231018.010101';
(select timestamp(fileman)'3231017.010101' union all select timestamp(fileman)'3231017.010101') union all (select timestamp(fileman)'3231016.020101' union all select timestamp(fileman)'3231016.020101');
select timestamp(fileman)'3231017.010101' union select timestamp(fileman)'3231016.020101' union select timestamp(fileman)'3231018.030101';
(select timestamp(fileman)'3231017.010101' union select timestamp(fileman)'3231017.010101') union (select timestamp(fileman)'3231016.020101' union select timestamp(fileman)'3231016.020101');
(select timestamp(fileman)'3231017.010101' intersect select timestamp(fileman)'3231017.010101') intersect (select timestamp(fileman)'3231017.010101' union select timestamp(fileman)'3231016.020101');
(select timestamp(fileman)'3231017.010101' union select timestamp(fileman)'3231019.030000' union select timestamp(fileman)'3231020.040000') except (select timestamp(fileman)'3231017.010101' union select timestamp(fileman)'3231016.020101');
-- sub query
select (select dob from testts limit 1);
-- table.*
select testts.* from testts;

-- Basic update
update testd set dob=date(fileman)'3241017' where dob=date(fileman)'3231017';
update testd set dob=dob;

--update testt set tob=timestamp(fileman)'235959' where tob=timestamp(fileman)'020101';
--update testt set tob=tob;

update testts set dob=timestamp(fileman)'3241017.235959' where dob=timestamp(fileman)'3231016.020101';
update testts set dob=dob;
select * from testd;
--select * from testt;
select * from testts;

-- Basic delete
delete from testd where dob=date(fileman)'3241017';
--delete from testt where tob=timestamp(fileman)'235959';
delete from testts where dob=timestamp(fileman)'3241017.235959';
select * from testd;
--select * from testt;
select * from testts;

-- Constraint checking
create table testdc (id int, dob date UNIQUE, dod date CHECK(dod > date(fileman)'3231018'));
insert into testdc values(1,date(fileman)'3231017',date(fileman)'3231018');
insert into testdc values(1,date(fileman)'3231017',date(fileman)'3231019');
insert into testdc values(1,date(fileman)'3231016',date(fileman)'3231019');
insert into testdc values(1,date(fileman)'3231016',date(fileman)'3231019');
update testdc set dod=date(fileman)'3231017';
select * from testdc;
--create table testtc (id int, tob time UNIQUE, tod time CHECK(tod < timestamp(fileman)'230000'));
--insert into testtc values(1,timestamp(fileman)'010101',timestamp(fileman)'230001');
--insert into testtc values(1,timestamp(fileman)'010101',timestamp(fileman)'220000');
--insert into testtc values(1,timestamp(fileman)'010201',timestamp(fileman)'220000');
--insert into testtc values(1,timestamp(fileman)'010201',timestamp(fileman)'220000');
--update testtc set tod=timestamp(fileman)'230001';
--select * from testtc;
create table testtsc (id int, tob timestamp UNIQUE, tod timestamp CHECK(tod < timestamp(fileman)'3231018.230000'));
insert into testtsc values(1,timestamp(fileman)'3231017.010101',timestamp(fileman)'3231018.230001');
insert into testtsc values(1,timestamp(fileman)'3231017.010101',timestamp(fileman)'3231017.220000');
insert into testtsc values(1,timestamp(fileman)'3231016.010201',timestamp(fileman)'3231017.220000');
insert into testtsc values(1,timestamp(fileman)'3231016.010201',timestamp(fileman)'3231017.220000');
update testtsc set tod=timestamp(fileman)'3231018.230000';
select * from testtsc;

-- Basic Views
create view testdv as select * from testdc;
--create view testtv as select * from testtc;
create view testtsv as select * from testtsc;
select * from testdv;
--select * from testtv;
select * from testtsv;
