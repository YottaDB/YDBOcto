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

-- TBT12 : OCTO867 : Test that t/f literals are treated as BOOLEAN or VARCHAR depending on context

select 't' union select 'g';
select true union select 't' union select 'f';
select 't' union select 'f' union select 'a';
select 't' union select 'f' union select 't';
select 't' union (select 'f' union select true);
select 't' union select false union select true;
select id=2 from names union select 'f' from names;

select 'abcd','t' union select 'efgh',false;
select 'abcd','t' union select 'efgh','f';
select true,'t' union select 'f','f';
select true,'t' union select 'f',false;
select 't','t' union select false,false;
select 't','t' union select 'f',false;
select 't','t' union select false,'f';
select 't','g' union select false,'f';

select id=2 from names union values (false);

select case when id=1 then 't' else 'f' end from names;

select case 'll' when 't' then 'MyZero' end;
select case true when 't' then 'MyZero' end;
select case 1=1 when 't' then 'MyZero' end;
select case 'g' when 't' then 'MyZero' end;

select 'a' = case true when false then 't' when true then 'f' end;
select 'a' = case true when false then 't' when true then 'f' end;
select 'a' = case true when false then 't' when true then 'f' end;
select 'a' = case true when false then 't' when true then 'f' else 'f' end;
select 'a' = case true when false then '' when null then null when true then 'f' else 't' end;
select 'a' = case true when false then '' when null then null when true then 'f' else null end;
select 'a' = case true when 'f' then 't' when null then 'f' end;
select 'a' = case when true then 't' when 't' then 't' when null then 'f' end;

select * from names where true = (select 't' union select true);

select * from names where 'f' is false;
select * from names where false = 'f';
select * from names where 't' = 'f';
select * from names where 'a' = 'b';
select * from names where 't';
select * from names where 't' or 'f';
select firstname,count(lastname) from names group by firstname having 't';
select firstname,count(lastname) from names group by firstname having 't' or 'f';
select * from names n1 inner join names n2 on 't';
select * from names n1 inner join names n2 on 't' or 'f';

select * from names where 'a' = 'f';
select * from names where 'f' = 'f';
select * from names where 't' = 'f';

select * from names where not 't';
select not 't' from names;

select greatest('t','f');
select least('t',false);

select true in (false, true, 'f');
select true in (false, 't', null);
select 't' in (false, 'f');
select true in ('f');
select true in ('f', 'f');

select * from names order by 'f'::boolean;

values ('f'),('a');
values ('f'),('t');

create table TBT12 (txt boolean);
insert into TBT12 values ('t'), (true), ('f'), (false), (null);
select * from TBT12;
update TBT12 set txt = 't' where txt = false;
update TBT12 set txt = 'f' where 't';
update TBT12 set txt = 't' where 'f' or 'f';
update TBT12 set txt = 'f' where not 't';
select * from TBT12;
delete from TBT12 where 't';
delete from TBT12 where 'f' or 'f';
delete from TBT12 where not 'f';
drop table TBT12;

create table TBT12 (txt varchar);
insert into TBT12 values ('t'), ('f');
select * from TBT12;
drop table TBT12;

create table TBT12 (txt varchar);
insert into TBT12 values ('t'), ('g'), ('f');
select * from TBT12;
drop table TBT12;

values (false, 'f'), (true, 't');
values (false, 'f'), (true, true);
values ('abcd', 'f'), ('efgh', 't');
values ('abcd', 'f'), ('efgh', true);
values ('f', 'f'), ('abcd', true);

select * from names where 't' AND false;

select * from names where (firstname = 'Zero') in (values ('f'::boolean));

select NULLIF(true, 't');
select NULLIF(false, 'f');
select NULLIF('t', true);
select NULLIF('f', false);
select NULLIF(true, 'f');
select NULLIF(false, 't');
select NULLIF('f', true);
select NULLIF('t', false);
select NULLIF('t', null);
select NULLIF('t', '');
select NULLIF(null, 'f');

select concat('t', 'f');
select concat('a', 'f');
select concat('f', '');
select concat('', 't');
select concat('f', null);
select concat(null, 't');

create table TBT12 (id integer, check ('t'));
insert into TBT12 values (1);
select * from TBT12;
drop table TBT12;

create table TBT12 (id integer, check (true in (false, true, 'f')));
insert into TBT12 values (2);
drop table TBT12;

create table TBT12 (id integer, check (true in (false, 't', null)));
insert into TBT12 values (3);
drop table TBT12;

create table TBT12 (id integer, check (NULLIF('t', true)));
insert into TBT12 values (4);
drop table TBT12;

create table TBT12 (id integer, CHECK ('MyZero' = case 'll' when 't' then 'MyZero' end));
insert into TBT12 values (1);
drop table TBT12;

create table TBT12 (id integer, CHECK ('MyZero' = case true when 't' then 'MyZero' end));
insert into TBT12 values (1);
drop table TBT12;

create table TBT12 (id integer, CHECK ('MyZero' = case 1=1 when 't' then 'MyZero' end));
insert into TBT12 values (1);
drop table TBT12;

create table TBT12 (id integer, CHECK ('MyZero' = case 'g' when 't' then 'MyZero' end));
insert into TBT12 values (1);
drop table TBT12;

create table TBT12 (id integer, CHECK ('a' = case true when 'f' then 't' when null then 'f' end));
insert into TBT12 values (1);
drop table TBT12;

