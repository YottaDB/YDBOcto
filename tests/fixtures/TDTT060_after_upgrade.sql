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

select * from tdtt060;
select * from tdtt060_1;
select * from tdtt060_1;
select * from tdtt060v;
select * from tdtt060v1;
select * from tdtt060v2;
select * from tdtt060v3;
select * from tdtt060v4;
select * from tdtt060v5;
select * from tdtt060v6;
select tdtt060fd(date'2023-01-01');
select tdtt060ft(time'01:01:01');
select tdtt060fts(timestamp'2023-01-01 01:01:01');
select tdtt060ftstz(timestamp with time zone'2023-01-01 01:01:01-05');
select tdtt060ftstz(timestamp with time zone'2023-04-04 01:01:01-04');
-- select * from tdtt060extract;
insert into tdtt060constraint values(2,date'2023-01-01');
select * from tdtt060constraint;
