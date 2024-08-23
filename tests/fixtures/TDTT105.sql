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

drop table if exists datep;
drop table if exists dater;
create table datep (dob date primary key, id integer) global "^datetimepdate" readonly;
create table dater (id integer primary key, dob date) global "^datetimedate" readonly;
select * from datep where dob = date'2023-01-02';
select * from dater where dob = date'2023-01-02';
select * from datep n1,dater n2 where n1.dob = n2.dob;
-- Tests IN key fix optimization when date/time column is primary key
select * from datep where dob in (date(fileman)'3230101');
-- Tests IN key fix optimization when date/time column is not a primary key
select * from dater where dob in (date(fileman)'3230101');
