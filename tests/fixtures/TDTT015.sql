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

select date'01-01-2023'||'test';
select date'01-01-2023'||NULL;
select time'01:01:01'||'test';
select time'01:01:01'||NULL;
select timestamp'01-01-2023 01:01:01' || NULL;
select timestamp'01-01-2023 01:01:01' || 'test';

create table test (dob date(horolog), dot time(horolog), dots timestamp(horolog));
insert into test values(date(horolog)'66749',time(horolog)'50531',timestamp(horolog)'66749,50531');
select * from test;
select dob||'test' from test;
select dot ||'test' from test;
select dots||'test' from test;
select dob||NULL from test;
select dot ||NULL from test;
select dots||NULL from test;

