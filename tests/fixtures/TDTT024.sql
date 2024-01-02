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

-- text
drop table if exists testd;
create table testd (id integer, dob date);
insert into testd values(1,date'10-17-2023');
insert into testd values(2,date'10-16-2023');
select * from testd;

drop table if exists testt;
create table testt (id integer, tob time);
insert into testt values(1,time'01:01:01');
insert into testt values(2,time'02:01:01');
select * from testt;

drop table if exists testts;
create table testts(id integer, dob timestamp);
insert into testts values(1, timestamp'10-17-2023 01:01:01');
insert into testts values(2, timestamp'10-16-2023 02:01:01');
select * from testts;

-- Basic select
select * from testd;
select * from testt;
select * from testts;

select dob,firstname from testd,names;
select tob,firstname from testt,names;
select dob,firstname from testts,names;

select date'10-17-2023',firstname from names limit 1;
select time'01:01:01',firstname from names limit 1;
select timestamp'10-16-2023 02:01:01',firstname from names limit 1;

select dob from testd group by dob;
select dob from testd order by dob;
select dob from testd limit 1;
select distinct dob from testd;
select count(dob) from testd;
select count(distinct dob) from testd;
select distinct dob from testd group by dob order by dob limit 1;
select distinct date'10-17-2023' from testd group by 1 order by 1 limit 1;
-- Values
select column1 from (values(date'10-17-2023'));
values(date'10-17-2023');
-- set operation
select * from (select date'10-17-2023' union select date'10-16-2023');
select * from (select dob from testd union select dob from testd);
select date'10-17-2023' union select date'10-16-2023';
select date'10-17-2023' union all select date'10-16-2023' union all select date'10-18-2023';
(select date'10-17-2023' union all select date'10-17-2023') union all (select date'10-16-2023' union all select date'10-16-2023');
select date'10-17-2023' union select date'10-16-2023' union select date'10-18-2023';
(select date'10-17-2023' union select date'10-17-2023') union (select date'10-16-2023' union select date'10-16-2023');
(select date'10-17-2023' intersect select date'10-17-2023') intersect (select date'10-17-2023' union select date'10-16-2023');
(select date'10-17-2023' union select date'10-19-2023' union select date'10-20-2023') except (select date'10-17-2023' union select date'10-16-2023');

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
select distinct timestamp'10-17-2023 01:01:01' from testts group by 1 order by 1 limit 1;
-- Values
select column1 from (values(timestamp'10-17-2023 01:01:01'));
values(timestamp'10-17-2023 01:01:01');
-- set operation
select * from (select timestamp'10-17-2023 01:01:01' union select timestamp'10-16-2023 02:01:01');
select * from (select dob from testts union select dob from testts);
select timestamp'10-17-2023 01:01:01' union select timestamp'10-16-2023 02:01:01';
select timestamp'10-17-2023 01:01:01' union all select timestamp'10-16-2023 02:01:01' union all select timestamp'10-18-2023 01:01:01';
(select timestamp'10-17-2023 01:01:01' union all select timestamp'10-17-2023 01:01:01') union all (select timestamp'10-16-2023 02:01:01' union all select timestamp'10-16-2023 02:01:01');
select timestamp'10-17-2023 01:01:01' union select timestamp'10-16-2023 02:01:01' union select timestamp'10-18-2023 03:01:01';
(select timestamp'10-17-2023 01:01:01' union select timestamp'10-17-2023 01:01:01') union (select timestamp'10-16-2023 02:01:01' union select timestamp'10-16-2023 02:01:01');
(select timestamp'10-17-2023 01:01:01' intersect select timestamp'10-17-2023 01:01:01') intersect (select timestamp'10-17-2023 01:01:01' union select timestamp'10-16-2023 02:01:01');
(select timestamp'10-17-2023 01:01:01' union select timestamp'10-19-2023 03:00:00' union select timestamp'10-20-2023 04:00:00') except (select timestamp'10-17-2023 01:01:01' union select timestamp'10-16-2023 02:01:01');
-- sub query
select (select dob from testts limit 1);
-- table.*
select testts.* from testts;

-- Basic update
update testd set dob=date'10-17-2024' where dob=date'10-17-2023';
update testd set dob=dob;

update testt set tob=time'23:59:59' where tob=time'02:01:01';
update testt set tob=tob;

update testts set dob=timestamp'10-17-2024 23:59:59' where dob=timestamp'10-16-2023 02:01:01';
update testts set dob=dob;
select * from testd;
select * from testt;
select * from testts;

-- Basic delete
delete from testd where dob=date'10-17-2024';
delete from testt where tob=time'23:59:59';
delete from testts where dob=timestamp'10-17-2024 23:59:59';
select * from testd;
select * from testt;
select * from testts;

-- Constraint checking
create table testdc (id int, dob date UNIQUE, dod date CHECK(dod > date'10-18-2023'));
insert into testdc values(1,date'10-17-2023',date'10-18-2023');
insert into testdc values(1,date'10-17-2023',date'10-19-2023');
insert into testdc values(1,date'10-16-2023',date'10-19-2023');
insert into testdc values(1,date'10-16-2023',date'10-19-2023');
update testdc set dod=date'10-17-2023';
select * from testdc;
create table testtc (id int, tob time UNIQUE, tod time CHECK(tod < time'23:00:00'));
insert into testtc values(1,time'01:01:01',time'23:00:01');
insert into testtc values(1,time'01:01:01',time'22:00:00');
insert into testtc values(1,time'01:02:01',time'22:00:00');
insert into testtc values(1,time'01:02:01',time'22:00:00');
update testtc set tod=time'23:00:01';
select * from testtc;
create table testtsc (id int, tob timestamp UNIQUE, tod timestamp CHECK(tod < timestamp'10-18-2023 23:00:00'));
insert into testtsc values(1,timestamp'10-17-2023 01:01:01',timestamp'10-18-2023 23:00:01');
insert into testtsc values(1,timestamp'10-17-2023 01:01:01',timestamp'10-17-2023 22:00:00');
insert into testtsc values(1,timestamp'10-16-2023 01:02:01',timestamp'10-17-2023 22:00:00');
insert into testtsc values(1,timestamp'10-16-2023 01:02:01',timestamp'10-17-2023 22:00:00');
update testtsc set tod=timestamp'10-18-2023 23:00:00';
select * from testtsc;

-- Basic Views
create view testdv as select * from testdc;
create view testtv as select * from testtc;
create view testtsv as select * from testtsc;
select * from testdv;
select * from testtv;
select * from testtsv;
