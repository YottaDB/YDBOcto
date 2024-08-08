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

select * from tdtt081;
select * from tdtt081_1;
select * from tdtt081_1;
select * from tdtt081v;
select tdtt081fd(time with time zone'01:01:01-05');
insert into tdtt081constraint values(2,time with time zone'07:01:01-05');
select * from tdtt081constraint;
