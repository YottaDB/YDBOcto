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

create table test (id integer primary key, dob date) global "^datetimedate" readonly;
select * from test;

create table test1 (id integer primary key, dob date) global "^datetimedate(keys(""id""))" readonly;
select * from test1;

-- create table test2 (id integer, dob date) global "^datetimedate(keys(""id""))" readonly; -- keys need to be added to `id` field when `keys(""id"")` is used
-- select * from test2;

create table test3 (id integer primary key, dob date global "^datetimedate(keys(""id""))") readonly;
select * from test3;

-- create table test4 (id integer, dob date global "^datetimedate(keys(""id""))") readonly;-- keys need to be added to `id` field when `keys(""id"")` is used
--select * from test4;

create table test5 (dob date primary key) global "^datetimedatekey" readonly;
select * from test5;

create table test6 (dob date primary key) global "^datetimedatekey(keys(""dob""))" readonly;
select * from test6;

create table test7 (dob date primary key START "2023-01-01" ENDPOINT "2023-01-02",id int) global "^datetimedatekey(keys(""dob""))"; -- This should be considered read-only because of START and ENDPOINT
select * from test7;

create table test8 (dob date primary key, id integer) global "^datetimedatekey" readonly;
select * from test8;

create table test9 (id integer primary key, dob date EXTRACT "$GET(^datetimedate(keys(""id"")))") global "^datetimedate"; -- This should be considered read-only because of EXTRACT usage
select * from test9;
