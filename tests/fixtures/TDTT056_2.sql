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

-- Readwrite table
drop table if exists TDTT0562;
create table TDTT0562 (id int,dob timestamp with time zone);
insert into TDTT0562 values(1,NULL);
insert into TDTT0562 values(0,timestamp with time zone'01-01-2023 01:01:01-05:00');
select * from TDTT0562;

drop table if exists TDTT0562;
create table TDTT0562 (id int,dob timestamp(zut) with time zone);
insert into TDTT0562 values(1,NULL);
insert into TDTT0562 values(0,timestamp(zut) with time zone'1700255690324907');
select * from TDTT0562;

drop table if exists TDTT0562;
create table TDTT0562 (id int,dob timestamp(zhorolog) with time zone);
insert into TDTT0562 values(1,NULL);
insert into TDTT0562 values(0,timestamp(zhorolog) with time zone'66795,58565,521473,18000');
select * from TDTT0562;

drop table if exists TDTT0562;
create table TDTT0562 (id int,dob timestamp(horolog) with time zone);
insert into TDTT0562 values(1,NULL);
insert into TDTT0562 values(0,timestamp(horolog) with time zone'66795,58565');
select * from TDTT0562;

drop table if exists TDTT0562;
create table TDTT0562 (id int,dob timestamp(fileman) with time zone);
insert into TDTT0562 values(1,NULL);
insert into TDTT0562 values(0,timestamp(fileman) with time zone'3230101.010101');
select * from TDTT0562;

-- Readonly table
drop table if exists TDTT0562;
create table TDTT0562 (id int primary key,dob timestamp) GLOBAL "^datetimetext" readonly;
select * from TDTT0562;

drop table if exists TDTT0562;
create table TDTT0562 (id int primary key,dob timestamp(zut)) GLOBAL "^datetimezut" readonly;
select * from TDTT0562;

drop table if exists TDTT0562;
create table TDTT0562 (id int primary key,dob timestamp(zhorolog)) GLOBAL "^datetimezhorolog" readonly;
select * from TDTT0562;

drop table if exists TDTT0562;
create table TDTT0562 (id int primary key,dob timestamp(horolog))GLOBAL "^datetimehorolog" readonly;
select * from TDTT0562;

drop table if exists TDTT0562;
create table TDTT0562 (id int primary key,dob timestamp(fileman))GLOBAL "^datetimefileman" readonly;
select * from TDTT0562;
