#################################################################
#								#
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TOB20 : OCTO959 : Test that ORDER BY on a KEY column with LIMIT on a huge table is optimized

-- Test ORDER BY optimization works with ENDPOINT/START/STARTINCLUDE keywords but not with END keyword

drop table if exists TOB20d;
create table TOB20d (id integer primary key, firstname varchar, lastname varchar) GLOBAL "^TOB20d";
insert into TOB20d values (12, 'abcd', 'efgh');
insert into TOB20d values (11, 'abcd', 'efgh');
insert into TOB20d values (10, 'abcd', 'efgh');
insert into TOB20d values (9, 'abcd', 'efgh');
insert into TOB20d values (8, 'abcd', 'efgh');
select * from TOB20d order by id;
select * from TOB20d order by id asc;
select * from TOB20d order by id asc limit 2;
select * from TOB20d order by id desc;
select * from TOB20d order by id desc limit 2;
select * from TOB20d where firstname = 'abcd' order by id asc limit 2;
select * from TOB20d where firstname = 'abcd' order by id desc limit 2;
create table TOB20e (id integer primary key START 9 STARTINCLUDE ENDPOINT 11, firstname varchar, lastname varchar) GLOBAL "^TOB20d" READONLY;
select * from TOB20e order by id;
select * from TOB20e order by id asc;
select * from TOB20e order by id asc limit 2;
select * from TOB20e order by id desc;
select * from TOB20e order by id desc limit 2;
select * from TOB20e where firstname = 'abcd' order by id asc limit 2;
select * from TOB20e where firstname = 'abcd' order by id desc limit 2;
create table TOB20f (id integer primary key START 9 STARTINCLUDE, firstname varchar, lastname varchar) GLOBAL "^TOB20d" READONLY;
select * from TOB20f order by id;
select * from TOB20f order by id asc;
select * from TOB20f order by id asc limit 2;
select * from TOB20f order by id desc;
select * from TOB20f order by id desc limit 2;
select * from TOB20f where lastname = 'efgh' order by id asc limit 2;
select * from TOB20f where lastname = 'efgh' order by id desc limit 2;
create table TOB20g (id integer primary key START 9 ENDPOINT 11, firstname varchar, lastname varchar) GLOBAL "^TOB20d" READONLY;
select * from TOB20g order by id;
select * from TOB20g order by id asc;
select * from TOB20g order by id asc limit 2;
select * from TOB20g order by id desc;
select * from TOB20g order by id desc limit 2;
select * from TOB20g where firstname = 'abcd' order by id asc limit 2;
select * from TOB20g where firstname = 'abcd' order by id desc limit 2;
create table TOB20h (id integer primary key START 9, firstname varchar, lastname varchar) GLOBAL "^TOB20d" READONLY;
select * from TOB20h order by id;
select * from TOB20h order by id asc;
select * from TOB20h order by id asc limit 2;
select * from TOB20h order by id desc;
select * from TOB20h order by id desc limit 2;
select * from TOB20h where lastname = 'efgh' order by id asc limit 2;
select * from TOB20h where lastname = 'efgh' order by id desc limit 2;

-- Test that END keyword disables ORDER BY optimization
create table TOB20i (id integer primary key END "keys(""id"")>11", firstname varchar, lastname varchar) GLOBAL "^TOB20d" READONLY;
select * from TOB20i order by id;
select * from TOB20i order by id asc;
select * from TOB20i order by id asc limit 2;
select * from TOB20i order by id desc;
select * from TOB20i order by id desc limit 2;
select * from TOB20i where lastname = 'efgh' order by id asc limit 2;
select * from TOB20i where lastname = 'efgh' order by id desc limit 2;

-- Test ORDER BY on VARCHAR/STRING key column with or without LIMIT keyword is NOT optimized
-- because of the presence of the MAYBE_CANONICAL keyword. Results would still be correct though.
drop table if exists TOB20c;
create table TOB20c (zipcode varchar primary key MAYBE_CANONICAL, residents integer);
insert into TOB20c values ('22960', 20);
insert into TOB20c values ('02169', 10);
select * from TOB20c order by zipcode;
select * from TOB20c order by zipcode desc;
select * from TOB20c order by zipcode desc limit 2;
select * from TOB20c order by zipcode asc limit 2;

-- Test ORDER BY on VARCHAR/STRING key column with or without LIMIT keyword is optimized
-- because of the absence of the MAYBE_CANONICAL keyword. But results would be incorrect.
drop table if exists TOB20j;
create table TOB20j (zipcode varchar primary key, residents integer);
insert into TOB20j values ('22960', 20);
insert into TOB20j values ('02169', 10);
select * from TOB20j order by zipcode;
select * from TOB20j order by zipcode desc;
select * from TOB20j order by zipcode desc limit 2;
select * from TOB20j order by zipcode asc limit 2;

