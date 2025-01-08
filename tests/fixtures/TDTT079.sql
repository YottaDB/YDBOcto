#################################################################
#								#
# Copyright (c) 2024-2025 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

select timestamp with time zone'2024-03-10 01:00:00-05' + time'01:00:00';
select timestamp with time zone'2024-03-10 02:00:00-05';
select timestamp with time zone'2024-03-10 02:00:00-05' + time'01:00:00';
select timestamp with time zone'2024-03-10 02:00:00-05'- time'01:00:00';
select timestamp with time zone'2024-03-10 03:00:00-05'- time'01:00:00';
select timestamp with time zone'2024-11-03 00:00:00-05';
select timestamp with time zone'2024-11-03 00:00:00-04';
select timestamp with time zone'2024-11-03 02:00:00-04';
select timestamp with time zone'2024-11-03 00:00:00-04'+ time'01:00:00';
select timestamp with time zone'2024-11-03 01:00:00-04'+ time'01:00:00';
select timestamp with time zone'2024-11-03 02:00:00-04'+ time'01:00:00';
select timestamp with time zone'2024-11-03 02:00:00-04'- time'01:00:00';
select timestamp with time zone'2024-11-03 03:00:00-04'- time'01:00:00';
select timestamp with time zone'2024-03-10 03:00:00';
select timestamp with time zone'2024-03-10 02:00:00';
select timestamp with time zone'2024-03-10 01:00:00';
select timestamp'2024-03-10 02:00:00'::timestamp with time zone;
select timestamp with time zone'2024-03-10 02:00:00'::timestamp with time zone;
select timestamp with time zone'2024-11-03 01:49:19-04' = timestamp with time zone'2024-11-03 01:49:19-05';
select timestamp with time zone'2194-11-02 01:49:19-04' = timestamp with time zone'2194-11-02 01:49:19-05';
select timestamp with time zone'2023-04-08 01:38:00+05:30'=timestamp'2023-04-07 16:08:00';
select least(timestamp with time zone'2194-11-02 01:49:19-04',timestamp with time zone'2194-11-02 01:49:19-05');
select greatest(timestamp with time zone'2194-11-02 01:49:19-04',timestamp with time zone'2194-11-02 01:49:19-05');
