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


-- horolog
-- Following are the values used in this test
-- Date to horolog date
-- 10-17-2023 -> 66764
-- 10-16-2023 -> 66763
-- 10-18-2023 -> 66765
-- 10-19-2023 -> 66766
-- 10-20-2023 -> 66767
-- 10-17-2024 -> 67130

-- Time to horolog time
-- 01:01:01 -> 3661
-- 02:01:01 -> 7261
-- 03:01:01 -> 10861
-- 03:00:00 -> 10800
-- 04:00:00 -> 14400
-- 23:59:59 -> 86399
-- 23:00:01 -> 82801
-- 22:00:00 -> 79200
-- 01:02:01 -> 3721
-- 23:00:00 -> 82800

-- Timestamp to horolog timestamp
-- 10-17-2023 01:01:01 -> 66764,3661
-- 10-16-2023 02:01:01 -> 66763,7261
-- 10-18-2023 01:01:01 -> 66765,3661
-- 10-18-2023 03:01:01 -> 66765,10861
-- 10-19-2023 03:00:00 -> 66766,10800
-- 10-20-2023 04:00:00 -> 66767,14400
-- 10-17-2024 23:59:59 -> 67130,86399
-- 10-16-2023 01:02:01 -> 66763,3721
-- 10-18-2023 23:00:01 -> 66765,82801
-- 10-17-2023 22:00:00 -> 66764,79200
-- 10-18-2023 23:00:00 -> 66765,82800
drop table if exists testd;
create table testd (id integer, dob date);
insert into testd values(1,date(horolog)'66764');
insert into testd values(2,date(horolog)'66763');
select * from testd;

drop table if exists testt;
create table testt (id integer, tob time);
insert into testt values(1,time(horolog)'3661');
insert into testt values(2,time(horolog)'7261');
select * from testt;

drop table if exists testts;
create table testts(id integer, dob timestamp);
insert into testts values(1, timestamp(horolog)'66764,3661');
insert into testts values(2, timestamp(horolog)'66763,7261');
select * from testts;

-- Basic select
select * from testd;
select * from testt;
select * from testts;

select dob,firstname from testd,names;
select tob,firstname from testt,names;
select dob,firstname from testts,names;

select date(horolog)'66764',firstname from names limit 1;
select time(horolog)'3661',firstname from names limit 1;
select timestamp(horolog)'66763,7261',firstname from names limit 1;

select dob from testd group by dob;
select dob from testd order by dob;
select dob from testd limit 1;
select distinct dob from testd;
select count(dob) from testd;
select count(distinct dob) from testd;
select distinct dob from testd group by dob order by dob limit 1;
select distinct date(horolog)'66764' from testd group by 1 order by 1 limit 1;
-- Values
select column1 from (values(date(horolog)'66764'));
values(date(horolog)'66764');
-- set operation
select * from (select date(horolog)'66764' union select date(horolog)'66763');
select * from (select dob from testd union select dob from testd);
select date(horolog)'66764' union select date(horolog)'66763';
select date(horolog)'66764' union all select date(horolog)'66763' union all select date(horolog)'66765';
(select date(horolog)'66764' union all select date(horolog)'66764') union all (select date(horolog)'66763' union all select date(horolog)'66763');
select date(horolog)'66764' union select date(horolog)'66763' union select date(horolog)'66765';
(select date(horolog)'66764' union select date(horolog)'66764') union (select date(horolog)'66763' union select date(horolog)'66763');
(select date(horolog)'66764' intersect select date(horolog)'66764') intersect (select date(horolog)'66764' union select date(horolog)'66763');
(select date(horolog)'66764' union select date(horolog)'66766' union select date(horolog)'66767') except (select date(horolog)'66764' union select date(horolog)'66763');

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
select distinct time(horolog)'3661' from testt group by 1 order by 1 limit 1;
-- Values
select column1 from (values(time(horolog)'3661'));
values(time(horolog)'3661');
-- set operation
select * from (select time(horolog)'3661' union select time(horolog)'7261');
select * from (select tob from testt union select tob from testt);
select time(horolog)'3661' union select time(horolog)'7261';
select time(horolog)'3661' union all select time(horolog)'7261' union all select time(horolog)'3661';
(select time(horolog)'3661' union all select time(horolog)'3661') union all (select time(horolog)'7261' union all select time(horolog)'7261');
select time(horolog)'3661' union select time(horolog)'7261' union select time(horolog)'10861';
(select time(horolog)'3661' union select time(horolog)'3661') union (select time(horolog)'7261' union select time(horolog)'7261');
(select time(horolog)'3661' intersect select time(horolog)'3661') intersect (select time(horolog)'3661' union select time(horolog)'7261');
(select time(horolog)'3661' union select time(horolog)'10800' union select time(horolog)'14400') except (select time(horolog)'3661' union select time(horolog)'7261');
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
select distinct timestamp(horolog)'66764,3661' from testts group by 1 order by 1 limit 1;
-- Values
select column1 from (values(timestamp(horolog)'66764,3661'));
values(timestamp(horolog)'66764,3661');
-- set operation
select * from (select timestamp(horolog)'66764,3661' union select timestamp(horolog)'66763,7261');
select * from (select dob from testts union select dob from testts);
select timestamp(horolog)'66764,3661' union select timestamp(horolog)'66763,7261';
select timestamp(horolog)'66764,3661' union all select timestamp(horolog)'66763,7261' union all select timestamp(horolog)'66765,3661';
(select timestamp(horolog)'66764,3661' union all select timestamp(horolog)'66764,3661') union all (select timestamp(horolog)'66763,7261' union all select timestamp(horolog)'66763,7261');
select timestamp(horolog)'66764,3661' union select timestamp(horolog)'66763,7261' union select timestamp(horolog)'66765,10861';
(select timestamp(horolog)'66764,3661' union select timestamp(horolog)'66764,3661') union (select timestamp(horolog)'66763,7261' union select timestamp(horolog)'66763,7261');
(select timestamp(horolog)'66764,3661' intersect select timestamp(horolog)'66764,3661') intersect (select timestamp(horolog)'66764,3661' union select timestamp(horolog)'66763,7261');
(select timestamp(horolog)'66764,3661' union select timestamp(horolog)'66766,10800' union select timestamp(horolog)'66767,14400') except (select timestamp(horolog)'66764,3661' union select timestamp(horolog)'66763,7261');
-- sub query
select (select dob from testts limit 1);
-- table.*
select testts.* from testts;

-- Basic update
update testd set dob=date(horolog)'67130' where dob=date(horolog)'66764';
update testd set dob=dob;

update testt set tob=time(horolog)'86399' where tob=time(horolog)'7261';
update testt set tob=tob;

update testts set dob=timestamp(horolog)'67130,86399' where dob=timestamp(horolog)'66763,7261';
update testts set dob=dob;
select * from testd;
select * from testt;
select * from testts;

-- Basic delete
delete from testd where dob=date(horolog)'67130';
delete from testt where tob=time(horolog)'86399';
delete from testts where dob=timestamp(horolog)'67130,86399';
select * from testd;
select * from testt;
select * from testts;

-- Constraint checking
create table testdc (id int, dob date UNIQUE, dod date CHECK(dod > date(horolog)'66765'));
insert into testdc values(1,date(horolog)'66764',date(horolog)'66765');
insert into testdc values(1,date(horolog)'66764',date(horolog)'66766');
insert into testdc values(1,date(horolog)'66763',date(horolog)'66766');
insert into testdc values(1,date(horolog)'66763',date(horolog)'66766');
update testdc set dod=date(horolog)'66764';
select * from testdc;
create table testtc (id int, tob time UNIQUE, tod time CHECK(tod < time(horolog)'82800'));
insert into testtc values(1,time(horolog)'3661',time(horolog)'82801');
insert into testtc values(1,time(horolog)'3661',time(horolog)'79200');
insert into testtc values(1,time(horolog)'3721',time(horolog)'79200');
insert into testtc values(1,time(horolog)'3721',time(horolog)'79200');
update testtc set tod=time(horolog)'82801';
select * from testtc;
create table testtsc (id int, tob timestamp UNIQUE, tod timestamp CHECK(tod < timestamp(horolog)'66765,82800'));
insert into testtsc values(1,timestamp(horolog)'66764,3661',timestamp(horolog)'66765,82801');
insert into testtsc values(1,timestamp(horolog)'66764,3661',timestamp(horolog)'66764,79200');
insert into testtsc values(1,timestamp(horolog)'66763,3721',timestamp(horolog)'66764,79200');
insert into testtsc values(1,timestamp(horolog)'66763,3721',timestamp(horolog)'66764,79200');
update testtsc set tod=timestamp(horolog)'66765,82800';
select * from testtsc;

-- Basic Views
create view testdv as select * from testdc;
create view testtv as select * from testtc;
create view testtsv as select * from testtsc;
select * from testdv;
select * from testtv;
select * from testtsv;
