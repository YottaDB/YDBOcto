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

select timestamp'2023-01-01';
select timestamp with time zone'2023-01-01 01:01:01';
select timestamp with time zone'2023-01-01';

select timestamp(fileman)'3230101';
select timestamp(fileman)'3230101.';
select timestamp(fileman) with time zone'3230101';
select timestamp(fileman) with time zone'3230101.';

select timestamp(horolog)'66840';
select timestamp(horolog) with time zone'66840';

select timestamp(zhorolog)'66840,,,';
select timestamp(zhorolog) with time zone'66840,38891,920149,';

create table TDTT071t (id integer primary key, dob timestamp) global "^TDTT071t(keys(""id""))" readonly;
select * from TDTT071t;
create table TDTT071tz (id integer primary key, dob timestamp with time zone) global "^TDTT071tz(keys(""id""))" readonly;
select * from TDTT071tz;
create table TDTT071tf (id integer primary key, dob timestamp(fileman)) global "^TDTT071tf(keys(""id""))" readonly;
select * from TDTT071tf;
create table TDTT071tzf (id integer primary key, dob timestamp(fileman) with time zone) global "^TDTT071tzf(keys(""id""))" readonly;
select * from TDTT071tzf;
create table TDTT071th (id integer primary key, dob timestamp(horolog)) global "^TDTT071th(keys(""id""))" readonly;
select * from TDTT071th;
create table TDTT071tzh (id integer primary key, dob timestamp(horolog) with time zone) global "^TDTT071tzh(keys(""id""))" readonly;
select * from TDTT071tzh;
create table TDTT071tzho (id integer primary key, dob timestamp(zhorolog)) global "^TDTT071tzho(keys(""id""))" readonly;
select * from TDTT071tzho;
create table TDTT071tzzho (id integer primary key, dob timestamp(zhorolog) with time zone) global "^TDTT071tzzho(keys(""id""))" readonly;
select * from TDTT071tzzho;


