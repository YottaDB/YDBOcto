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

-- The gvn layout for the following queries are extracted in TCF035 to TCF035.ext
-- The layout is not expected to change. Octo's dependency logic between tables and functions
-- depend on this layout. If there is a change then the dependency gvns might be lost after
-- an upgrade.
-- Note: The .ext file is an extract of `oid` and dependency related gvns.  Just the dependency gvn layout extract should
-- be sufficient to fail this test, `oid` related gvn extract is also done just to double check that the gvn layout has not
-- changed.
--	Any change in this gvn layout will result in the loss of dependency nodes created prior
--	to the layout change. This test is present to alert us on such an occasion and to inform us to check auto upgarde cases.
-- OID layout: ^%ydboctoocto("functions","ABS","%ydboctoF0BXSKYtCkTUXzFwpivpB68","oid")="213"
-- Example dependency layout: ^%ydboctoocto("functions","ABS","%ydboctoF0BXSKYtCkTUXzFwpivpB68","check_constraint","TEST","TEST_ID_CHECK")=""

-- Return type integer
-- parmless
create function samevalue1() returns integer as $$^PARMLESSFUNC;
create table test1 (id int check(id < samevalue1()));
-- integer
create function samevalue1(integer) returns integer as $$samevalue^functions;
create table test2 (id int check(id < samevalue1(2)));
-- boolean
create function samevalue1(boolean) returns integer as $$samevalue^functions;
create table test3 (id int check(id < samevalue1(true)));
-- numeric
create function samevalue1(numeric) returns integer as $$samevalue^functions;
create table test4 (id int check(id < samevalue1(1.1)));
-- varchar
create function samevalue1(varchar) returns integer as $$samevalue^functions;
create table test5 (id int check(id < samevalue1('test')));

-- Return type boolean
-- parmless
create function samevalue2() returns boolean as $$^PARMLESSFUNC;
create table test6 (id int check(TRUE < samevalue2()));
-- integer
create function samevalue2(integer) returns boolean as $$samevalue^functions;
create table test7 (id int check(TRUE < samevalue2(2)));
-- boolean
create function samevalue2(boolean) returns boolean as $$samevalue^functions;
create table test8 (id int check(TRUE < samevalue2(true)));
-- numeric
create function samevalue2(numeric) returns boolean as $$samevalue^functions;
create table test9 (id int check(TRUE < samevalue2(1.1)));
-- varchar
create function samevalue2(varchar) returns boolean as $$samevalue^functions;
create table test10 (id int check(TRUE < samevalue2('test')));

-- Return type numeric
-- parmless
create function samevalue3() returns numeric as $$^PARMLESSFUNC;
create table test11 (id int check(1.1 < samevalue3()));
-- integer
create function samevalue3(integer) returns numeric as $$samevalue^functions;
create table test12 (id int check(1.1 < samevalue3(1)));
-- boolean
create function samevalue3(boolean) returns numeric as $$samevalue^functions;
create table test13 (id int check(1.1 < samevalue3(true)));
-- numeric
create function samevalue3(numeric) returns numeric as $$samevalue^functions;
create table test14 (id int check(1.1 < samevalue3(1.1)));
-- varchar
create function samevalue3(varchar) returns numeric as $$samevalue^functions;
create table test15 (id int check(1.1 < samevalue3('test')));

-- Return type varchar
-- parmless
create function samevalue4() returns varchar as $$^PARMLESSFUNC;
create table test16 (id int check('test' < samevalue4()));
-- integer
create function samevalue4(integer) returns varchar as $$samevalue^functions;
create table test17 (id int check('test' < samevalue4(1)));
-- boolean
create function samevalue4(boolean) returns varchar as $$samevalue^functions;
create table test18 (id int check('test' < samevalue4(true)));
-- numeric
create function samevalue4(numeric) returns varchar as $$samevalue^functions;
create table test19 (id int check('test' < samevalue4(1.1)));
-- varchar
create function samevalue4(varchar) returns varchar as $$samevalue^functions;
create table test20 (id int check('test' < samevalue4('test')));
