#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TDFT01 : OCTO54 : Test of simple DELETE FROM queries in names database (one primary key column)

drop table if exists TDFT01;
create table TDFT01 (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30));
insert into TDFT01 (select * FROM names);

select * from TDFT01;
-- also test AS when specified implicitly (n1 below) and used in WHERE clause
delete from TDFT01 n1 where n1.lastname != 'Cool';
select * from TDFT01;
insert into TDFT01 (select * FROM names where lastname != 'Cool');

select * from TDFT01;
delete from TDFT01 where lastname != 'Cool';
select * from TDFT01;
insert into TDFT01 (select * FROM names where lastname != 'Cool');

select * from TDFT01;
-- also test AS when specified implicitly (n1 below) and not used in WHERE clause
delete from TDFT01 n1 where lastname = 'Cool';
select * from TDFT01;
insert into TDFT01 (select * FROM names where lastname = 'Cool');

select * from TDFT01;
delete from TDFT01;
select * from TDFT01;
insert into TDFT01 (select * FROM names);

select * from TDFT01;
delete from TDFT01 where lastname is NULL;
select * from TDFT01;
insert into TDFT01 (select * FROM names where lastname is NULL);

select * from TDFT01;
delete from TDFT01 where lastname is NULL OR firstname = 'Zero';
select * from TDFT01;
insert into TDFT01 (select * FROM names where lastname is NULL OR firstname = 'Zero');

select * from TDFT01;
delete from TDFT01 where lastname = 'Cool' OR firstname = 'Zero';
select * from TDFT01;
insert into TDFT01 (select * FROM names where lastname = 'Cool' OR firstname = 'Zero');

select * from TDFT01;
delete from TDFT01 where lastname != 'Cool' AND id > 3;
select * from TDFT01;
insert into TDFT01 (select * FROM names where lastname != 'Cool' AND id > 3);

select * from TDFT01;
delete from TDFT01 where lastname is NULL;
select * from TDFT01;
insert into TDFT01 (select * FROM names where lastname is NULL);

select * from TDFT01;
-- test delete from works with subqueries in WHERE clause
-- also test AS works when specified explicitly and used in WHERE clause
delete from TDFT01 as n1 where EXISTS (select * FROM names n2 where n2.id = n1.id + 2);
select * from TDFT01;
insert into TDFT01 (select * FROM names n1 where EXISTS (select * from names n2 where n2.id = n1.id + 2));

select * from TDFT01;
insert into TDFT01 VALUES((SAMEVALUE(6)), 'First6', 'Last6');
delete from TDFT01 where lastname = 'Last6';
insert into TDFT01 VALUES(7, 'First7', 'Last7');
delete from TDFT01 where lastname = SAMEVALUE('Last7');
insert into TDFT01 VALUES(8, SAMEVALUE('First8'), 'Last8');
delete from TDFT01 where lastname = 'Last8';

