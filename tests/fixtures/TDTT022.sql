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

-- Following queries are not expected to have internal to output format conversion routine
select id,firstname from names;
select distinct lastname,firstname from names limit 3;
select distinct lastname,firstname from names limit 3;
select distinct lastname,firstname from names group by lastname, firstname order by lastname,firstname limit 3;
select distinct lastname,firstname from names group by lastname, firstname limit 3;
select (select distinct firstname from names group by firstname limit 1);
select (select distinct firstname from names group by firstname order by firstname limit 1);

-- Creating a dummy test table with date type column
create table test (dob date);
insert into test values(date'01-01-2023');
-- Following queries are expected to have internal to output format conversion routines
select dob from test;
select distinct dob from test group by dob limit 1;
select distinct dob from test group by dob order by dob limit 1;
select date'01-01-2023';
select date'01-01-2023' group by 1;
select date'01-01-2023' group by 1 order by 1;
select (select n2.dob from test n1 limit 1) from test n2;
select date'01-01-2023'+1;
select dob,firstname from test,names;

