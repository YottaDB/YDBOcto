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


drop table if exists testd;
create table testd (id integer, dob date);
insert into testd values(1,date'2023-10-17');
insert into testd values(2,date'2023-10-16');
select * from testd;

drop table if exists testts;
create table testts(id integer, dob timestamp);
insert into testts values(1, timestamp'2023-10-17 01:01:01');
insert into testts values(2, timestamp'2023-10-16 02:01:01');
select * from testts;
