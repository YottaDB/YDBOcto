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

drop table if exists TDTT0561;
create table TDTT0561 (dob date);
insert into TDTT0561 values(NULL);
select * from TDTT0561;

drop table if exists TDTT0561;
create table TDTT0561 (dob time);
insert into TDTT0561 values(NULL);
select * from TDTT0561;

drop table if exists TDTT0561;
create table TDTT0561 (dob time with time zone);
insert into TDTT0561 values(NULL);
select * from TDTT0561;

drop table if exists TDTT0561;
create table TDTT0561 (dob timestamp);
insert into TDTT0561 values(NULL);
select * from TDTT0561;

drop table if exists TDTT0561;
create table TDTT0561 (dob timestamp with time zone);
insert into TDTT0561 values(NULL);
select * from TDTT0561;

-- Drop all tables such that hello_db doesn't failing these exist in Postgres
drop table TDTT0561;
