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

create table TDTT061 (id integer primary key, dob date);
insert into TDTT061 values (2, (select '2023-12-31'));
insert into TDTT061 values (3, '3231231'); -- This needs to fail
select * from TDTT061;

drop table TDTT061;
create table TDTT061 (id integer primary key, dob date(fileman));
insert into TDTT061 values (1, '3231231'); -- This needs insert a fileman date
select * from TDTT061;
