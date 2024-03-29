#################################################################
#								#
# Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TC059 : OCTO519 : Double-quoted strings are treated as case sensitive identifiers in CREATE/DROP TABLE statements

-- Identifiers as column names
create table abcd ("id" integer);
select * from abcd;
select "id" from abcd;
select id from abcd;
select "ID" from abcd; -- Error: Unknown column 'ID'
select ID from abcd;
drop table abcd;

create table abcd ("ID" integer);
select * from abcd;
select "id" from abcd; -- Error: Unknown column 'id'
select id from abcd; -- Error: Unknown column 'id'
select "ID" from abcd;
select ID from abcd; -- Error: Unknown column 'id'
drop table "ABCD"; -- Error: Unknown table 'ABCD'
drop table "abcd";

create table abcd (id integer);
select * from abcd;
select "id" from abcd;
select id from abcd;
select "ID" from abcd; -- Error: Unknown column 'ID'
select ID from abcd;
drop table ABCD;

create table abcd (ID integer);
select * from abcd;
select "id" from abcd;
select id from abcd;
select "ID" from abcd; -- Error: Unknown column 'ID'
select ID from abcd;
select abcd.ID from abcd;
select abcd."ID" from abcd; -- Error: Unknown column 'ID'
select abcd."id" from abcd;
select abcd.id from abcd;
select "abcd".ID from abcd;
select abcd.ID from abcd;
select "ABCD".ID from abcd; -- Error: Missing FROM clause
select ABCD.ID from abcd;
drop table "ABCD"; -- Error: Unknown table
drop table abcd; -- Table dropped successfully

-- Identifiers as table names
create table "EFGH" ("ID" integer);
select * from "efgh"; -- Error: Unknown table
select * from efgh; -- Error: Unknown table
select * from "EFGH";
select * from EFGH; -- Error: Unknown table
select * from "EFGH" where "EFGH"."ID" = 0;
select * from "Efgh" where "EFGH"."ID" = 0; -- Error: Unknown table
select * from "EFGH" where "EFGH"."Id" = 0; -- Error: Unknown column
select * from "EFGH" where efgh.id = 0; -- Error: Missing FROM-clause
drop table efgh; -- Error: Unknown table
drop table EFGH; -- Error: Unknown table
drop table "efgh"; -- Error: Unknown table
drop table "EFGH"; -- Table dropped successfully

create table "efgh" (id integer);
select * from "EFGH"; -- Error: Unknown table
select * from efgh;
select * from "EFGH";
select * from EFGH;
drop table "EFGH"; -- Error: Unknown table
drop table "efgh"; -- Table dropped successfully

create table efgh ("id" integer);
select * from "efgh";
select * from efgh;
select * from "EFGH"; -- Error: Unknown table
select * from EFGH;
drop table "EFGH"; -- Error: Unknown table
drop table EFGH; -- Table dropped successfully

create table EFGH (ID integer);
select * from "efgh";
select * from efgh;
select * from "EFGH"; -- Error: Unknown table
select * from EFGH;
drop table "EFGH"; -- Error: Unknown table
drop table EFGH; -- Table dropped successfully

-- Identifiers and literals as alias names
-- select 1 as ID;
-- select 1 as 'id';
-- select 1 as 'ID';
-- select 1 as "ID";
-- select 1 as "id";

-- Correct handling of double quotes in GROUP BY
drop table if exists tmp;
create table tmp ("ID1" integer, "ID2" integer);
select "ID1" from tmp group by "ID2";

-- Correct handling of double quotes in NATURAL JOIN
drop table if exists tmp;
create table tmp (id1 integer, id2 integer) READWRITE;
insert into tmp values (1,2);
insert into tmp values (2,1);
select * from (select id1 as "ID" from tmp) n1 natural join (select id2 as "ID" from tmp) n2;
select n2."ID", n1."ID" from (select id1 as "ID" from tmp) n1 natural join (select id2 as "ID" from tmp) n2;
select n2.id, n1.id from (select id1 as "ID" from tmp) n1 natural join (select id2 as "ID" from tmp) n2; -- Error: Unknown column

-- Correct handling of column names that differ only in case sensitivity
drop table if exists tbl;
create table tbl (col integer, "COL" integer, primary key(col, "COL"));
select * from tbl;
drop table if exists tbl;
create table tbl (col integer, "COL" integer, primary key(col, "COL")) GLOBAL "^tbl(keys(""col""),keys(""COL""))";
select * from tbl;
-- ERR_GLOBAL_KEY_COLS_ORDER error for mismatch between `keys(..)` expression and column name, e.g. `COL` to `keys(""col"")`
drop table if exists tbl;
create table tbl (col integer, "COL" integer, primary key(col, "COL")) GLOBAL "^tbl(keys(""COL""),keys(""COL""))";

