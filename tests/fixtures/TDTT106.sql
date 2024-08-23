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

drop table if exists dater;
create table dater (id integer primary key , dobf date key num 1, dobl date) global "^datetimedate" readonly;
select * from dater;
-- No key fixing because of not being able to apply tranformation functions
select * from dater where dobf=date'2023-01-02';
select * from dater where dobf=date'2023-01-03';
select * from dater where id=0;

drop table if exists test;
create table test (id integer primary key, fname varchar key num 1, lname varchar) global "^test" readonly;
select * from test;
-- No cross references are created for primary key and key num columns but key fixing does happen
select * from test where fname='first';
select * from test where fname='first1';
select * from test where id =0;


