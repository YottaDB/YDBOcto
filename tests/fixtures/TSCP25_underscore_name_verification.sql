#################################################################
#                                                              #
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.      #
# All rights reserved.                                         #
#                                                              #
#      This source code contains the intellectual property     #
#      of its copyright holder(s), and is made available       #
#      under a license.  If you do not know the terms of       #
#      the license, please stop and do not read further.       #
#                                                              #
#################################################################

-- TSCP25 : OCTO993 : Allow underscore as starting character in identifiers
drop function if exists _testf;
create function _testf(integer) returns integer as $$samevalue^functions;
select _testf(1);
select 1 order by _testf(1);
select _testf(1) as _ group by _ order by _ ;

drop function if exists __;
create function __(integer) returns integer as $$samevalue^functions;
select __(1);
select 1 order by __(1);
select __(1) as ___ group by ___ order by ___;

-- Copied from TSCP25.sql to validate column names in these cases
select id as _ from names group by _ order by _;
select id as __ from names group by __ order by __;

drop table if exists _test1;
create table _test1 (_id1 integer);
insert into _test1 values(1);
insert into _test1(_id1) values(2);
select _id1 from _test1;
select _id1 as _ from _test1 group by _ order by _;
\d _test1;

drop table if exists __;
create table __ (__ integer);
insert into __ values(1);
insert into __(__) values(2);
select __ from __;
select __ as ___ from __ group by ___ order by ___;
\d __;

drop table if exists _test2;
create table _test2 (_id1 integer constraint _constraint check(_id1<1));
\d _test2;
insert into _test2 values(0);
insert into _test2 values(1); -- Error
select * from _test2;

drop table if exists _names keepdata;
CREATE TABLE _names (_id INTEGER PRIMARY KEY, _firstName VARCHAR(30), lastName VARCHAR(30)) GLOBAL "^names";
select * from _names;

drop table if exists _test3 keepdata;
create table _test3 (_id1 integer primary key, _fullname text extract "$PIECE(^names(keys(""_ID1"")),""|"",1)_$PIECE(^names(keys(""_ID1"")),""|"",2)") GLOBAL "^names";
select * from _test3;

drop table _test3 keepdata;
create table _test3 (_id1 integer primary key START 0 END "keys(""_ID1"")=3", _firstname text, _lastname text) GLOBAL "^names";
select * from _test3;

drop table _test3 keepdata;
create table _test3 (_id1 integer primary key START 0 ENDPOINT 3, _firstname text, _lastname text) GLOBAL "^names";
select * from _test3;

drop table _test3 keepdata;
create table _test3 (_id1 integer primary key, _firstname text, _lastname text, _fullname text extract "(values(""_FIRSTNAME""))_(values(""_LASTNAME""))") GLOBAL "^names";
select * from _test3;

drop table _test3 keepdata;
create table _test3 (_id1 integer primary key, _firstname text, _lastname text) DELIM "_" GLOBAL "^x";
select * from _test3;
select * from _test3 order by _id1 desc;

drop table _test3 keepdata;
create table _test3 (_id1 integer primary key, _firstname text, _lastname text) DELIM "_" GLOBAL "^x(keys(""_ID1""))";
select * from _test3;
select * from _test3 order by _id1 desc;

-- Error
select id as _ from names group by _ having _ > 1;
select id as __ from names group by __ having __ > 1 order by __;

create function x(int) returns int as $$_samevalue^functions;
create function x(int) returns int as $$_samevalue^functions;
create function x(int) returns int as $$^%_ydboctofCONCAT;
create function x(int) returns int as $$^_%ydboctofCONCAT;
