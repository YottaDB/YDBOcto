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

-- In-exact dates
select date(fileman)'3230000';
select date(fileman)'3230100';
select date(fileman)'3230001'; -- This should result in an error
select date(fileman)'323'; -- This should result in an error
create table test (id int primary key, dob date(fileman)) global "^datefileman"readonly;
select * from test;
create table testerror (id int primary key, dob date(fileman)) global "^datefilemanerror"readonly;
select * from testerror;

-- Edge case timestamp
select timestamp(fileman)'2960714.24';
select timestamp(fileman)'2960714.23';
select timestamp(fileman)'2960731.24';
select timestamp(fileman)'2991231.24';
select timestamp(fileman)'2991231.25';
select timestamp(fileman)'323';
select timestamp(fileman)'2960124.16263'; -- second is having a single digit
select timestamp(fileman)'2960124.16265'; -- second is having a single digit
select timestamp(fileman)'2960124.16266'; -- second is having a single digit error case
select timestamp(fileman)'2960124.16267'; -- second is having a single digit error case
select timestamp(fileman)'2960124.163'; -- minute is having a single digit
select timestamp(fileman)'2960124.165'; -- minute is having a single digit
select timestamp(fileman)'2960124.166'; -- minute is having a single digit error case
select timestamp(fileman)'2960124.167'; -- minute is having a single digit error case
select timestamp(fileman)'2960124.1';
select timestamp(fileman)'2960124.2';
select timestamp(fileman)'2960124.3'; -- hour is having a single digit error case
select timestamp(fileman)'2960124.9'; -- hour is having a single digit error case
select timestamp(fileman)'2960124.01';
select timestamp(fileman)'2960124.10'; -- error
select timestamp(fileman)'2960124.100';
select timestamp(fileman)'2960124.1000';
select timestamp(fileman)'2960124.10000';
select timestamp(fileman)'2960124.100000';
select timestamp(fileman)'2960124.20'; -- error
select timestamp(fileman)'2960124.200';
select timestamp(fileman)'2960124.2000';
select timestamp(fileman)'2960124.20000';
select timestamp(fileman)'2960124.200000';
create table testtimestamp (id int primary key, dob timestamp(fileman)) global "^timestampfileman"readonly;
select * from testtimestamp;
create table testtimestamperror1 (id int primary key, dob timestamp(fileman)) global "^timestampfilemanerror1"readonly;
create table testtimestamperror2 (id int primary key, dob timestamp(fileman)) global "^timestampfilemanerror2"readonly;
create table testtimestamperror3 (id int primary key, dob timestamp(fileman)) global "^timestampfilemanerror3"readonly;
create table testtimestamperror4 (id int primary key, dob timestamp(fileman)) global "^timestampfilemanerror4"readonly;
create table testtimestamperror5 (id int primary key, dob timestamp(fileman)) global "^timestampfilemanerror5"readonly;
create table testtimestamperror6 (id int primary key, dob timestamp(fileman)) global "^timestampfilemanerror6"readonly;
create table testtimestamperror7 (id int primary key, dob timestamp(fileman)) global "^timestampfilemanerror7"readonly;
create table testtimestamperror8 (id int primary key, dob timestamp(fileman)) global "^timestampfilemanerror8"readonly;
create table testtimestamperror9 (id int primary key, dob timestamp(fileman)) global "^timestampfilemanerror9"readonly;
select * from testtimestamperror1;
select * from testtimestamperror2;
select * from testtimestamperror3;
select * from testtimestamperror4;
select * from testtimestamperror5;
select * from testtimestamperror6;
select * from testtimestamperror7;
select * from testtimestamperror8;
select * from testtimestamperror9;
