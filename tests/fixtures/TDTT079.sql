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

select timestamp with time zone'03-10-2024 01:00:00-05' + time'01:00:00';
select timestamp with time zone'03-10-2024 02:00:00-05';
select timestamp with time zone'03-10-2024 02:00:00-05' + time'01:00:00';
select timestamp with time zone'03-10-2024 02:00:00-05'- time'01:00:00';
select timestamp with time zone'03-10-2024 03:00:00-05'- time'01:00:00';
select timestamp with time zone'11-03-2024 00:00:00-05';
select timestamp with time zone'11-03-2024 00:00:00-04';
select timestamp with time zone'11-03-2024 02:00:00-04';
select timestamp with time zone'11-03-2024 00:00:00-04'+ time'01:00:00';
select timestamp with time zone'11-03-2024 01:00:00-04'+ time'01:00:00';
select timestamp with time zone'11-03-2024 02:00:00-04'+ time'01:00:00';
select timestamp with time zone'11-03-2024 02:00:00-04'- time'01:00:00';
select timestamp with time zone'11-03-2024 03:00:00-04'- time'01:00:00';
