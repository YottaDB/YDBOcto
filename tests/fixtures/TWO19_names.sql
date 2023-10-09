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

-- Test ORDER BY on INTEGER key column with key-fixing optimization in WHERE clause
-- This tests the "is_string" = true code paths in tmpl_tablejoin.ctemplate.
-- Below queries are obtained by adding an ORDER BY to queries in TWO19_octoonly.sql
select * from names where id = 3 order by id;
select * from names where id = 3 order by id desc;
select * from names where id = 3 order by id limit 2;
select * from names where id = 3 order by id desc limit 2;
select * from names where id > 3 order by id asc;
select * from names where id > 3 order by id desc;
select * from names where id > 3 order by id asc limit 2;
select * from names where id > 3 order by id desc limit 2;
select * from names where id < 3 order by id;
select * from names where id < 3 order by id desc;
select * from names where id < 3 order by id limit 2;
select * from names where id < 3 order by id desc limit 2;
select * from names where id >= 3 order by id asc;
select * from names where id >= 3 order by id desc;
select * from names where id >= 3 order by id asc limit 2;
select * from names where id >= 3 order by id desc limit 2;
select * from names where id <= 3 order by id;
select * from names where id <= 3 order by id desc;
select * from names where id <= 3 order by id limit 2;
select * from names where id <= 3 order by id desc limit 2;
select * from names where firstname = 'Joey' order by id asc;
select * from names where firstname = 'Joey' order by id desc;
select * from names where firstname = 'Joey' order by id asc limit 2;
select * from names where firstname = 'Joey' order by id desc limit 2;
select * from names where firstname > 'Joey' order by id;
select * from names where firstname > 'Joey' order by id desc;
select * from names where firstname > 'Joey' order by id limit 2;
select * from names where firstname > 'Joey' order by id desc limit 2;
select * from names where firstname < 'Joey' order by id asc;
select * from names where firstname < 'Joey' order by id desc;
select * from names where firstname < 'Joey' order by id asc limit 2;
select * from names where firstname < 'Joey' order by id desc limit 2;
select * from names where firstname >= 'Joey' order by id;
select * from names where firstname >= 'Joey' order by id desc;
select * from names where firstname >= 'Joey' order by id limit 2;
select * from names where firstname >= 'Joey' order by id desc limit 2;
select * from names where firstname <= 'Joey' order by id asc;
select * from names where firstname <= 'Joey' order by id desc;
select * from names where firstname <= 'Joey' order by id asc limit 2;
select * from names where firstname <= 'Joey' order by id desc limit 2;

-- Test ORDER BY on VARCHAR/STRING key column with key-fixing optimization in WHERE clause
-- ORDER BY on INTEGER key column is tested in TWO19_names.sql
-- This tests the "is_string" = true code paths in tmpl_tablejoin.ctemplate.
drop table if exists TWO19a;
create table TWO19a (name varchar primary key, age integer);
insert into TWO19a values ('abcd', 20);
insert into TWO19a values ('efgh', 10);
insert into TWO19a values ('ijkl', 55);
insert into TWO19a values ('mnop', 35);
select * from TWO19a where name > 'efgh' order by name;
select * from TWO19a where name > 'efgh' order by name desc;
select * from TWO19a where name > 'efgh' order by name desc limit 2;
select * from TWO19a where name > 'efgh' order by name asc limit 2;
select * from TWO19a where name < 'efgh' order by name;
select * from TWO19a where name < 'efgh' order by name desc;
select * from TWO19a where name < 'efgh' order by name desc limit 2;
select * from TWO19a where name < 'efgh' order by name asc limit 2;
select * from TWO19a where name >= 'efgh' order by name;
select * from TWO19a where name >= 'efgh' order by name desc;
select * from TWO19a where name >= 'efgh' order by name desc limit 2;
select * from TWO19a where name >= 'efgh' order by name asc limit 2;
select * from TWO19a where name <= 'efgh' order by name;
select * from TWO19a where name <= 'efgh' order by name desc;
select * from TWO19a where name <= 'efgh' order by name desc limit 2;
select * from TWO19a where name <= 'efgh' order by name asc limit 2;

