#################################################################
#								#
# Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TUT002 : OCTO579 : Test of simple UPDATE queries in names database (one primary key column)

drop table if exists TUT002;
create table TUT002 (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30));
insert into TUT002 (select * FROM names);

select * from TUT002;
-- also test AS when specified implicitly (n1 below) and used in WHERE clause
update TUT002 n1 set firstname = lastname, lastname = firstname where n1.lastname != 'Cool';
select * from TUT002;
-- restore TUT002 to be a copy of names
update TUT002 n1 set firstname = lastname, lastname = firstname where n1.lastname != 'Cool';

select * from TUT002;
update TUT002 set firstname = lastname, lastname = firstname where lastname != 'Cool';
select * from TUT002;
-- restore TUT002 to be a copy of names
update TUT002 set firstname = lastname, lastname = firstname where lastname != 'Cool';

select * from TUT002;
-- also test AS when specified implicitly (n1 below) and not used in WHERE clause
update TUT002 n1 set firstname = lastname, lastname = firstname where lastname = 'Cool';
select * from TUT002;
-- restore TUT002 to be a copy of names
update TUT002 set firstname = lastname, lastname = firstname where firstname = 'Cool';

select * from TUT002;
update TUT002 set firstname = lastname, lastname = firstname;
select * from TUT002;
-- restore TUT002 to be a copy of names
update TUT002 set firstname = lastname, lastname = firstname;

select * from TUT002;
update TUT002 set firstname = lastname, lastname = firstname where lastname is NULL;
select * from TUT002;
-- restore TUT002 to be a copy of names
update TUT002 set firstname = lastname, lastname = firstname where firstname is NULL;

select * from TUT002;
update TUT002 set firstname = lastname, lastname = firstname where lastname is NULL OR firstname = 'Zero';
select * from TUT002;
-- restore TUT002 to be a copy of names
update TUT002 set firstname = lastname, lastname = firstname where firstname is NULL OR lastname = 'Zero';

select * from TUT002;
update TUT002 set firstname = lastname, lastname = firstname where lastname = 'Cool' OR firstname = 'Zero';
select * from TUT002;
-- restore TUT002 to be a copy of names
update TUT002 set firstname = lastname, lastname = firstname where firstname = 'Cool' OR lastname = 'Zero';

select * from TUT002;
update TUT002 set firstname = lastname, lastname = firstname where lastname != 'Cool' AND id > 3;
select * from TUT002;
-- restore TUT002 to be a copy of names
update TUT002 set firstname = lastname, lastname = firstname where lastname != 'Cool' AND id > 3;

select * from TUT002;
update TUT002 set firstname = lastname, lastname = firstname where lastname is NULL;
select * from TUT002;
-- restore TUT002 to be a copy of names
update TUT002 set firstname = lastname, lastname = firstname where firstname is NULL;

select * from TUT002;
-- test delete from works with subqueries in WHERE clause
-- also test AS works when specified explicitly and used in WHERE clause
update TUT002 as n1 set firstname = lastname, lastname = firstname where EXISTS (select * FROM names n2 where n2.id = n1.id + 2);
select * from TUT002;
-- restore TUT002 to be a copy of names
update TUT002 as n1 set firstname = lastname, lastname = firstname where EXISTS (select * FROM names n2 where n2.id = n1.id + 2);

select * from TUT002;
insert into TUT002 VALUES((SAMEVALUE(6)), 'First6', 'Last6');
update TUT002 set firstname = lastname, lastname = firstname where lastname = 'Last6';
select * from TUT002;
insert into TUT002 VALUES(7, 'First7', 'Last7');
update TUT002 set firstname = lastname, lastname = firstname where lastname = SAMEVALUE('Last7');
select * from TUT002;
insert into TUT002 VALUES(8, SAMEVALUE('First8'), 'Last8');
update TUT002 set firstname = lastname, lastname = firstname where lastname = 'Last8';
select * from TUT002;

-- Test that UPDATE works with type cast operator (without that operator it issued a ERR_TYPE_MISMATCH error)
update TUT002 set firstname = lastname, lastname = id::text; -- this query without a type cast is tested in TUT001.sql
select * from TUT002;

-- Test UPDATE with table containing 1 key column and 1 non-key column
create table TUT002B (id INTEGER PRIMARY KEY, firstName VARCHAR(30));
insert into TUT002B (select id, firstname FROM names);
update TUT002B set firstname = firstname || '#';
select * from TUT002;

-- Test of https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/579#note_1031578433
create table TUT002C (t2 varchar primary key);
insert into TUT002C values ('M1');
insert into TUT002C values ('T2');
insert into TUT002C values ('Z3');
select * from TUT002C;
update TUT002C set t2 = t2 || 'a';
select * from TUT002C;
drop table TUT002C;

