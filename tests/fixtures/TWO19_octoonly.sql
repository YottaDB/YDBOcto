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

-- TWO19 : OCTO616 : Key fixing optimization in WHERE clause for COLUMN > LITERAL (or >=, <, <=)

-- Test = with primary key column
select * from names where id = 3;
select * from names where id = 3 limit 2;

-- Test > with primary key column
select * from names where id > 3;
select * from names where id > 3 limit 2;

-- Test < with primary key column
select * from names where id < 3;
select * from names where id < 3 limit 2;

-- Test >= with primary key column
select * from names where id >= 3;
select * from names where id >= 3 limit 2;

-- Test <= with primary key column
select * from names where id <= 3;
select * from names where id <= 3 limit 2;

-- Test = with non-key column
select * from names where firstname = 'Joey';
select * from names where firstname = 'Joey' limit 2;

-- Test > with non-key column
select * from names where firstname > 'Joey';
select * from names where firstname > 'Joey' limit 2;

-- Test < with non-key column
select * from names where firstname < 'Joey';
select * from names where firstname < 'Joey' limit 2;

-- Test >= with non-key column
select * from names where firstname >= 'Joey';
select * from names where firstname >= 'Joey' limit 2;

-- Test <= with non-key column
select * from names where firstname <= 'Joey';
select * from names where firstname <= 'Joey' limit 2;

-- Test = with non-first-key column in a composite key table
select * from composite where id6 = 7;
select * from composite where id6 = 7 limit 2;

-- Test > with non-first-key column in a composite key table
select * from composite where id6 > 7;
select * from composite where id6 > 7 limit 2;

-- Test < with non-first-key column in a composite key table
select * from composite where id6 < 7;
select * from composite where id6 < 7 limit 2;

-- Test >= with non-first-key column in a composite key table
select * from composite where id6 >= 7;
select * from composite where id6 >= 7 limit 2;

-- Test <= with non-first-key column in a composite key table
select * from composite where id6 <= 7;
select * from composite where id6 <= 7 limit 2;

-- Test that ORDER BY optimization is enabled in case of primary key column
select * from names where id in (3,1,5) order by id asc;
select * from names where id in (3,1,5) order by id desc;

-- Test that ORDER BY optimization is disabled if non-key column is used without =
-- (search for LP_BOOLEAN_EQUALS check in lp_optimize_order_by.c and the surrounding comment for WHY).
select * from names where firstname > 'Joey' order by id desc;
select * from names where firstname > 'Joey' order by id asc;
select * from names where firstname >= 'Joey' order by id desc;
select * from names where firstname >= 'Joey' order by id asc;
select * from names where firstname < 'Joey' order by id desc;
select * from names where firstname < 'Joey' order by id asc;
select * from names where firstname <= 'Joey' order by id desc;
select * from names where firstname <= 'Joey' order by id asc;

-- Test key-fixing optimization in WHERE clause (YDBOcto#616) with START, STARTINCLUDE, ENDPOINT keywords
-- and with/without ORDER BY optimization (YDBOcto#959).
create table TWO19b (id integer primary key, firstname varchar, lastname varchar) GLOBAL "^TWO19b";
insert into TWO19b values (13, 'abcd0', 'efgh11');
insert into TWO19b values (12, 'abcd1', 'efgh6');
insert into TWO19b values (11, 'abcd2', 'efgh7');
insert into TWO19b values (10, 'abcd3', 'efgh8');
insert into TWO19b values (9, 'abcd4', 'efgh9');
insert into TWO19b values (8, 'abcd5', 'efgh10');
insert into TWO19b values (7, 'abcd12', 'efgh13');
-- Test with START
create table TWO19c (id integer primary key START 8, firstname varchar, lastname varchar) GLOBAL "^TWO19b";
select * from TWO19c where id > 6 order by id asc limit 2;
select * from TWO19c where id > 6 order by id desc limit 2;
select * from TWO19c where id >= 6 order by id asc limit 2;
select * from TWO19c where id >= 6 order by id desc limit 2;
select * from TWO19c where id > 8 order by id asc limit 2;
select * from TWO19c where id > 8 order by id desc limit 2;
select * from TWO19c where id >= 8 order by id asc limit 2;
select * from TWO19c where id >= 8 order by id desc limit 2;
select * from TWO19c where id > 9 order by id asc limit 2;
select * from TWO19c where id > 9 order by id desc limit 2;
select * from TWO19c where id >= 9 order by id asc limit 2;
select * from TWO19c where id >= 9 order by id desc limit 2;
select * from TWO19c where id < 12 order by id asc limit 2;
select * from TWO19c where id < 12 order by id desc limit 2;
select * from TWO19c where id <= 12 order by id asc limit 2;
select * from TWO19c where id <= 12 order by id desc limit 2;
select * from TWO19c where id < 14 order by id asc limit 2;
select * from TWO19c where id < 14 order by id desc limit 2;
select * from TWO19c where id <= 14 order by id asc limit 2;
select * from TWO19c where id <= 14 order by id desc limit 2;
-- Test with START,STARTINCLUDE
create table TWO19d (id integer primary key START 8 STARTINCLUDE, firstname varchar, lastname varchar) GLOBAL "^TWO19b";
select * from TWO19d where id > 6 order by id asc limit 2;
select * from TWO19d where id > 6 order by id desc limit 2;
select * from TWO19d where id >= 6 order by id asc limit 2;
select * from TWO19d where id >= 6 order by id desc limit 2;
select * from TWO19d where id > 8 order by id asc limit 2;
select * from TWO19d where id > 8 order by id desc limit 2;
select * from TWO19d where id >= 8 order by id asc limit 2;
select * from TWO19d where id >= 8 order by id desc limit 2;
select * from TWO19d where id > 9 order by id asc limit 2;
select * from TWO19d where id > 9 order by id desc limit 2;
select * from TWO19d where id >= 9 order by id asc limit 2;
select * from TWO19d where id >= 9 order by id desc limit 2;
select * from TWO19d where id < 12 order by id asc limit 2;
select * from TWO19d where id < 12 order by id desc limit 2;
select * from TWO19d where id <= 12 order by id asc limit 2;
select * from TWO19d where id <= 12 order by id desc limit 2;
select * from TWO19d where id < 14 order by id asc limit 2;
select * from TWO19d where id < 14 order by id desc limit 2;
select * from TWO19d where id <= 14 order by id asc limit 2;
select * from TWO19d where id <= 14 order by id desc limit 2;
-- Test with ENDPOINT
create table TWO19e (id integer primary key ENDPOINT 11, firstname varchar, lastname varchar) GLOBAL "^TWO19b";
select * from TWO19e where id > 6 order by id asc limit 2;
select * from TWO19e where id > 6 order by id desc limit 2;
select * from TWO19e where id >= 6 order by id asc limit 2;
select * from TWO19e where id >= 6 order by id desc limit 2;
select * from TWO19e where id > 8 order by id asc limit 2;
select * from TWO19e where id > 8 order by id desc limit 2;
select * from TWO19e where id >= 8 order by id asc limit 2;
select * from TWO19e where id >= 8 order by id desc limit 2;
select * from TWO19e where id > 9 order by id asc limit 2;
select * from TWO19e where id > 9 order by id desc limit 2;
select * from TWO19e where id >= 9 order by id asc limit 2;
select * from TWO19e where id >= 9 order by id desc limit 2;
select * from TWO19e where id < 12 order by id asc limit 2;
select * from TWO19e where id < 12 order by id desc limit 2;
select * from TWO19e where id <= 12 order by id asc limit 2;
select * from TWO19e where id <= 12 order by id desc limit 2;
select * from TWO19e where id < 14 order by id asc limit 2;
select * from TWO19e where id < 14 order by id desc limit 2;
select * from TWO19e where id <= 14 order by id asc limit 2;
select * from TWO19e where id <= 14 order by id desc limit 2;
-- Test with START,ENDPOINT
create table TWO19f (id integer primary key START 8 ENDPOINT 11, firstname varchar, lastname varchar) GLOBAL "^TWO19b";
select * from TWO19f where id > 6 order by id asc limit 2;
select * from TWO19f where id > 6 order by id desc limit 2;
select * from TWO19f where id >= 6 order by id asc limit 2;
select * from TWO19f where id >= 6 order by id desc limit 2;
select * from TWO19f where id > 8 order by id asc limit 2;
select * from TWO19f where id > 8 order by id desc limit 2;
select * from TWO19f where id >= 8 order by id asc limit 2;
select * from TWO19f where id >= 8 order by id desc limit 2;
select * from TWO19f where id > 9 order by id asc limit 2;
select * from TWO19f where id > 9 order by id desc limit 2;
select * from TWO19f where id >= 9 order by id asc limit 2;
select * from TWO19f where id >= 9 order by id desc limit 2;
select * from TWO19f where id < 12 order by id asc limit 2;
select * from TWO19f where id < 12 order by id desc limit 2;
select * from TWO19f where id <= 12 order by id asc limit 2;
select * from TWO19f where id <= 12 order by id desc limit 2;
select * from TWO19f where id < 14 order by id asc limit 2;
select * from TWO19f where id < 14 order by id desc limit 2;
select * from TWO19f where id <= 14 order by id asc limit 2;
select * from TWO19f where id <= 14 order by id desc limit 2;
-- Test with START,STARTINCLUDE,ENDPOINT
create table TWO19g (id integer primary key START 8 STARTINCLUDE ENDPOINT 11, firstname varchar, lastname varchar) GLOBAL "^TWO19b";
select * from TWO19g where id > 6 order by id asc limit 2;
select * from TWO19g where id > 6 order by id desc limit 2;
select * from TWO19g where id >= 6 order by id asc limit 2;
select * from TWO19g where id >= 6 order by id desc limit 2;
select * from TWO19g where id > 8 order by id asc limit 2;
select * from TWO19g where id > 8 order by id desc limit 2;
select * from TWO19g where id >= 8 order by id asc limit 2;
select * from TWO19g where id >= 8 order by id desc limit 2;
select * from TWO19g where id > 9 order by id asc limit 2;
select * from TWO19g where id > 9 order by id desc limit 2;
select * from TWO19g where id >= 9 order by id asc limit 2;
select * from TWO19g where id >= 9 order by id desc limit 2;
select * from TWO19g where id < 12 order by id asc limit 2;
select * from TWO19g where id < 12 order by id desc limit 2;
select * from TWO19g where id <= 12 order by id asc limit 2;
select * from TWO19g where id <= 12 order by id desc limit 2;
select * from TWO19g where id < 14 order by id asc limit 2;
select * from TWO19g where id < 14 order by id desc limit 2;
select * from TWO19g where id <= 14 order by id asc limit 2;
select * from TWO19g where id <= 14 order by id desc limit 2;

