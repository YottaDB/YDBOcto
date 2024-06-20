#################################################################
#								#
# Copyright (c) 2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

select date'2023-01-01 01:01:01';
select date(fileman)'3230101.010101';
select date(horolog)'67011,41907';
select date(zhorolog)'67011,41921,909140,14400';
select date(zut)'1718897933845272';

create table datef(id integer primary key, dob date(fileman)) global "^datef(keys(""id""))" readonly;
select * from datef;

create table dateh(id integer primary key, dob date(horolog)) global "^dateh(keys(""id""))" readonly;
select * from dateh;

create table datet(id integer primary key, dob date) global "^datet(keys(""id""))" readonly;
select * from datet;

select date(fileman)'3020730.11164601';
