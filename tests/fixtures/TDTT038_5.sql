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

-- Following are the values used in this test
-- Date to fileman date
-- 10-17-2023 -> 66764 -> 3231017
-- 10-16-2023 -> 66763 -> 3231016
-- 10-18-2023 -> 66765 -> 3231018
-- 10-19-2023 -> 66766 -> 3231019
-- 10-20-2023 -> 66767 -> 3231020
-- 10-17-2024 -> 67130 -> 3241017

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
-- 10-17-2023 01:01:01 -> 66764,3661 -> 3231017.010101
-- 10-16-2023 02:01:01 -> 66763,7261 -> 3231016.020101
-- 10-18-2023 01:01:01 -> 66765,3661 -> 3231018.010101
-- 10-18-2023 03:01:01 -> 66765,10861 -> 3231018.030101
-- 10-19-2023 03:00:00 -> 66766,10800 -> 3231019.030000
-- 10-20-2023 04:00:00 -> 66767,14400 -> 3231020.040000
-- 10-17-2024 23:59:59 -> 67130,86399 -> 3241017.235959
-- 10-16-2023 01:02:01 -> 66763,3721 -> 3231016.010201
-- 10-18-2023 23:00:01 -> 66765,82801 -> 3231018.230001
-- 10-17-2023 22:00:00 -> 66764,79200 -> 3231017.220000
-- 10-18-2023 23:00:00 -> 66765,82800 -> 3231018.230000
-- text
drop table if exists testd;
create table testd (id integer, dob date);
insert into testd values(1,date'17/10/2023');
insert into testd values(2,date'16/10/2023');
select * from testd;

drop table if exists testt;
create table testt (id integer, tob time);
insert into testt values(1,time'01:01:01');
insert into testt values(2,time'02:01:01');
select * from testt;

drop table if exists testts;
create table testts(id integer, dob timestamp);
insert into testts values(1, timestamp'17/10/2023 01:01:01');
insert into testts values(2, timestamp'16/10/2023 02:01:01');
select * from testts;

-- Basic select
select * from testd;
select * from testt;
select * from testts;

select dob,firstname from testd,names;
select tob,firstname from testt,names;
select dob,firstname from testts,names;

select date'17/10/2023',firstname from names limit 1;
select time'01:01:01',firstname from names limit 1;
select timestamp'16/10/2023 02:01:01',firstname from names limit 1;

select dob from testd group by dob;
select dob from testd order by dob;
select dob from testd limit 1;
select distinct dob from testd;
select count(dob) from testd;
select count(distinct dob) from testd;
select distinct dob from testd group by dob order by dob limit 1;
select distinct date'17/10/2023' from testd group by 1 order by 1 limit 1;
-- Values
select column1 from (values(date'17/10/2023'));
values(date'17/10/2023');
-- set operation
select * from (select date'17/10/2023' union select date'16/10/2023');
select * from (select dob from testd union select dob from testd);
select date'17/10/2023' union select date'16/10/2023';
select date'17/10/2023' union all select date'16/10/2023' union all select date'18/10/2023';
(select date'17/10/2023' union all select date'17/10/2023') union all (select date'16/10/2023' union all select date'16/10/2023');
select date'17/10/2023' union select date'16/10/2023' union select date'18/10/2023';
(select date'17/10/2023' union select date'17/10/2023') union (select date'16/10/2023' union select date'16/10/2023');
(select date'17/10/2023' intersect select date'17/10/2023') intersect (select date'17/10/2023' union select date'16/10/2023');
(select date'17/10/2023' union select date'19/10/2023' union select date'20/10/2023') except (select date'17/10/2023' union select date'16/10/2023');

-- sub query
select (select dob from testd limit 1);
-- table.*
select testd.* from testd;

select tob from testt group by tob;
select tob from testt order by tob;
select tob from testt limit 1;
select distinct tob from testt;
select count(tob) from testt;
select count(distinct tob) from testt;
select distinct tob from testt group by tob order by tob limit 1;
select distinct time'01:01:01' from testt group by 1 order by 1 limit 1;
-- Values
select column1 from (values(time'01:01:01'));
values(time'01:01:01');
-- set operation
select * from (select time'01:01:01' union select time'02:01:01');
select * from (select tob from testt union select tob from testt);
select time'01:01:01' union select time'02:01:01';
select time'01:01:01' union all select time'02:01:01' union all select time'01:01:01';
(select time'01:01:01' union all select time'01:01:01') union all (select time'02:01:01' union all select time'02:01:01');
select time'01:01:01' union select time'02:01:01' union select time'03:01:01';
(select time'01:01:01' union select time'01:01:01') union (select time'02:01:01' union select time'02:01:01');
(select time'01:01:01' intersect select time'01:01:01') intersect (select time'01:01:01' union select time'02:01:01');
(select time'01:01:01' union select time'03:00:00' union select time'04:00:00') except (select time'01:01:01' union select time'02:01:01');
-- sub query
select (select tob from testt limit 1);
-- table.*
select testt.* from testt;

select dob from testts group by dob;
select dob from testts order by dob;
select dob from testts limit 1;
select distinct dob from testts;
select count(dob) from testts;
select count(distinct dob) from testts;
select distinct dob from testts group by dob order by dob limit 1;
select distinct timestamp'17/10/2023 01:01:01' from testts group by 1 order by 1 limit 1;
-- Values
select column1 from (values(timestamp'17/10/2023 01:01:01'));
values(timestamp'17/10/2023 01:01:01');
-- set operation
select * from (select timestamp'17/10/2023 01:01:01' union select timestamp'16/10/2023 02:01:01');
select * from (select dob from testts union select dob from testts);
select timestamp'17/10/2023 01:01:01' union select timestamp'16/10/2023 02:01:01';
select timestamp'17/10/2023 01:01:01' union all select timestamp'16/10/2023 02:01:01' union all select timestamp'18/10/2023 01:01:01';
(select timestamp'17/10/2023 01:01:01' union all select timestamp'17/10/2023 01:01:01') union all (select timestamp'16/10/2023 02:01:01' union all select timestamp'16/10/2023 02:01:01');
select timestamp'17/10/2023 01:01:01' union select timestamp'16/10/2023 02:01:01' union select timestamp'18/10/2023 03:01:01';
(select timestamp'17/10/2023 01:01:01' union select timestamp'17/10/2023 01:01:01') union (select timestamp'16/10/2023 02:01:01' union select timestamp'16/10/2023 02:01:01');
(select timestamp'17/10/2023 01:01:01' intersect select timestamp'17/10/2023 01:01:01') intersect (select timestamp'17/10/2023 01:01:01' union select timestamp'16/10/2023 02:01:01');
(select timestamp'17/10/2023 01:01:01' union select timestamp'19/10/2023 03:00:00' union select timestamp'20/10/2023 04:00:00') except (select timestamp'17/10/2023 01:01:01' union select timestamp'16/10/2023 02:01:01');
-- sub query
select (select dob from testts limit 1);
-- table.*
select testts.* from testts;

-- Basic update
update testd set dob=date'17/10/2023' where dob=date'17/10/2023';
update testd set dob=dob;

update testt set tob=time'23:59:59' where tob=time'02:01:01';
update testt set tob=tob;

update testts set dob=timestamp'17/10/2023 23:59:59' where dob=timestamp'16/10/2023 02:01:01';
update testts set dob=dob;
select * from testd;
select * from testt;
select * from testts;

-- Basic delete
delete from testd where dob=date'17/10/2023';
delete from testt where tob=time'23:59:59';
delete from testts where dob=timestamp'17/10/2023 23:59:59';
select * from testd;
select * from testt;
select * from testts;

-- Constraint checking
drop table if exists testdc;
create table testdc (id int, dob date UNIQUE, dod date CHECK(dod > date'18/10/2023'));
insert into testdc values(1,date'17/10/2023',date'18/10/2023');
insert into testdc values(1,date'17/10/2023',date'19/10/2023');
insert into testdc values(1,date'16/10/2023',date'19/10/2023');
insert into testdc values(1,date'16/10/2023',date'19/10/2023');
update testdc set dod=date'17/10/2023';
select * from testdc;
drop table if exists testtc;
create table testtc (id int, tob time UNIQUE, tod time CHECK(tod < time'23:00:00'));
insert into testtc values(1,time'01:01:01',time'23:00:01');
insert into testtc values(1,time'01:01:01',time'22:00:00');
insert into testtc values(1,time'01:02:01',time'22:00:00');
insert into testtc values(1,time'01:02:01',time'22:00:00');
update testtc set tod=time'23:00:01';
select * from testtc;
drop table if exists testtsc;
create table testtsc (id int, tob timestamp UNIQUE, tod timestamp CHECK(tod < timestamp'18/10/2023 23:00:00'));
insert into testtsc values(1,timestamp'17/10/2023 01:01:01',timestamp'18/10/2023 23:00:01');
insert into testtsc values(1,timestamp'17/10/2023 01:01:01',timestamp'17/10/2023 22:00:00');
insert into testtsc values(1,timestamp'16/10/2023 01:02:01',timestamp'17/10/2023 22:00:00');
insert into testtsc values(1,timestamp'16/10/2023 01:02:01',timestamp'17/10/2023 22:00:00');
update testtsc set tod=timestamp'18/10/2023 23:00:00';
select * from testtsc;

-- Basic Views
create view testdv as select * from testdc;
create view testtv as select * from testtc;
create view testtsv as select * from testtsc;
select * from testdv;
select * from testtv;
select * from testtsv;
-- Placing at the end so that other tests are not effected
drop view if exists testdv;
drop view if exists testtv;
drop view if exists testtsv;
