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

select -time'01:01:00'+date'2023-01-01';
select -time'01:01:00';
select -time with time zone'01:01:01 +1';

select EXISTS(select date'2023-01-01');
select EXISTS(select time'01:01:01');
select EXISTS(select time with time zone'01:01:01 +1');
select EXISTS(select timestamp'2023-01-01 01:01:01');
select EXISTS(select timestamp with time zone'2023-01-01 01:01:01 +1');
