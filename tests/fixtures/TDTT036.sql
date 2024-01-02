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

select * from names order by date'01-01-2023';
select * from names order by timestamp'01-01-2023 01:01:01';
select * from names order by time'01:01:01';
select * from names order by time with time zone'01:01:01';
select * from names order by timestamp with time zone'01-01-2023 01:01:01';

select 1 from names group by date'01-01-2023';
select 1 from names group by timestamp'01-01-2023 01:01:01';
select 1 from names group by time'01:01:01';
select 1 from names group by time with time zone'01:01:01';
select 1 from names group by timestamp with time zone'01-01-2023 01:01:01';

create table test1 (id int, dob date check(dob<date'01-01-2023'), tob time check(tob<time'01:01:01'),tos timestamp check(tos<timestamp'01-01-2023 01:01:01')
								, tobs time with time zone check (tobs<time with time zone'01:01:01')
								, toss timestamp with time zone check (toss< timestamp with time zone'01-01-2023 01:01:01'));
\d test1;
create table test2 (id int, dob date check(dob<date(fileman)'3230101'), tob time check(tob<time(horolog)'3661'));
\d test2;
create view tv as select date(fileman)'3230101';
\d tv;
create table t (id int,dob date(fileman));
\d t;
create table ta(tob time(horolog));
\d ta;
