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

create table test (id int,dob date);
insert into test values(1,date'2023-01-01');
insert into test values(1,date'2023-01-01');
select n1.* from test n1;
select 1 from test n1 group by n1.*;
select 1 from test n1 group by 1 order by n1.*;
select n1.dob from test n1;
select n1.* = n1.* from test n1;
