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


select time'16:12:00'; -- result should be same but no time zone information displayed

select time'22:30:00+05:30';

select timestamp with time zone'2023-11-07 16:12:00'; -- result should be same but with time zone of -5
select timestamp'2023-11-07 16:12:00'; -- result should be same
select timestamp with time zone'2023-11-07 22:30:00+05:30'; -- result will be '2023-11-07 12:00:00-05'
select timestamp'2023-11-07 22:30:00+05:30'; -- result '2023-11-07 22:30:00';
select timestamp with time zone'2023-11-07 10:30:00+05:30'; -- result '2023-11-07 00:00:00-05'

-- Edge cases
select timestamp with time zone'2023-09-11 12:01:41-05:30';
select timestamp with time zone'2023-09-11 11:01:41-04:00';
select timestamp with time zone'2023-11-09 12:01:41-05:00';
select timestamp with time zone'2023-09-11 12:01:41-05:00';
select timestamp with time zone'2023-12-08 01:38:00+05:30'=timestamp'2023-12-07 15:08:00';
select timestamp with time zone'2023-04-08 01:38:00+05:30'=timestamp'2023-04-07 16:08:00';
