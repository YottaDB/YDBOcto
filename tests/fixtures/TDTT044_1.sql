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

create table testr(id int primary key, dob date(fileman)) GLOBAL "^date" readonly;
select * from testr;
select * from testr where dob > date'02-01-2023';
select * from testr where dob between date'02-01-2023' and date(fileman)'3230301';

create table testrtext(id int primary key, dob date) GLOBAL "^datetext" readonly;
select * from testrtext;
select * from testrtext where dob > date'02-01-2023';
select * from testrtext where dob between date'02-01-2023' and date(fileman)'3230301';

create table testrtexttz(id int primary key, dob timestamp with time zone) GLOBAL "^tstexttz" readonly;
select * from testrtexttz;
select * from testrtexttz where dob > date'02-01-2023';
select * from testrtexttz where dob between date'02-01-2023' and date(fileman)'3230301';

set datestyle='YMD';
drop table if exists txt;
create table txt (id integer primary key, firstname varchar, lastname varchar, dateofbirth date) GLOBAL "^text" READONLY;
drop table if exists flmn;
create table flmn (id integer primary key, firstname varchar, lastname varchar, dateofbirth date(fileman)) GLOBAL "^fileman" READONLY;
select t.id as t_id, t.dateofbirth as t_dateofbirth from txt t order by dateofbirth;
select f.id as f_id, f.dateofbirth as f_dateofbirth from flmn f order by dateofbirth;
select t.id as t_id,f.id as f_id,t.dateofbirth,f.dateofbirth from txt t, flmn f where t.dateofbirth = f.dateofbirth;

drop table if exists orders;
create table orders (order_id INTEGER PRIMARY KEY, order_date DATE, order_amount VARCHAR(7), customer_id INTEGER) GLOBAL "^orders" READONLY;
set datestyle="mdy";
select * from orders where orders.order_date = DATE '03-14-1760';
select * from orders where orders.order_date IN (DATE '03-14-1760');

SELECT ALL orders.order_date FROM orders  WHERE (TRUE);
SELECT ALL orders.order_date FROM orders  WHERE (TRUE OR (DATE '03-14-1760' > orders.order_date));

create table timestamp_zut_tbl (order_id integer primary key, order_timestamp timestamp(zut)) GLOBAL "^timestampzuttbl" READONLY;
create table timestamp_fileman_tbl (order_id integer primary key, order_timestamp timestamp(fileman)) GLOBAL "^timestampfilemantbl" READONLY;
select order_timestamp from timestamp_zut_tbl order by order_timestamp;
select order_timestamp from timestamp_fileman_tbl order by order_timestamp;
select * from timestamp_zut_tbl t1 inner join timestamp_fileman_tbl t2 on t1.order_timestamp = t2.order_timestamp order by t1.order_timestamp;
