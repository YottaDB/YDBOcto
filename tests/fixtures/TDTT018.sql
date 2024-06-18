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

select date'2023-01-01' IS TRUE;
select date'2023-01-01' IS FALSE;
select date'2023-01-01' IS NULL;
select date'2023-01-01' IS UNKNOWN;
select date'2023-01-01' IS NOT TRUE;
select date'2023-01-01' IS NOT FALSE;
select date'2023-01-01' IS NOT NULL;
select date'2023-01-01' IS NOT UNKNOWN;

select time'01:01:01' IS TRUE;
select time'01:01:01' IS FALSE;
select time'01:01:01' IS NULL;
select time'01:01:01' IS UNKNOWN;
select time'01:01:01' IS NOT TRUE;
select time'01:01:01' IS NOT FALSE;
select time'01:01:01' IS NOT NULL;
select time'01:01:01' IS NOT UNKNOWN;

select time with time zone'01:01:01 +1' IS TRUE;
select time with time zone'01:01:01 +1' IS FALSE;
select time with time zone'01:01:01 +1' IS NULL;
select time with time zone'01:01:01 +1' IS UNKNOWN;
select time with time zone'01:01:01 +1' IS NOT TRUE;
select time with time zone'01:01:01 +1' IS NOT FALSE;
select time with time zone'01:01:01 +1' IS NOT NULL;
select time with time zone'01:01:01 +1' IS NOT UNKNOWN;

select timestamp'2023-01-01 01:01:01' IS TRUE;
select timestamp'2023-01-01 01:01:01' IS FALSE;
select timestamp'2023-01-01 01:01:01' IS NULL;
select timestamp'2023-01-01 01:01:01' IS UNKNOWN;
select timestamp'2023-01-01 01:01:01' IS NOT TRUE;
select timestamp'2023-01-01 01:01:01' IS NOT FALSE;
select timestamp'2023-01-01 01:01:01' IS NOT NULL;
select timestamp'2023-01-01 01:01:01' IS NOT UNKNOWN;

select timestamp with time zone'2023-01-01 01:01:01 +1' IS TRUE;
select timestamp with time zone'2023-01-01 01:01:01 +1' IS FALSE;
select timestamp with time zone'2023-01-01 01:01:01 +1' IS NULL;
select timestamp with time zone'2023-01-01 01:01:01 +1' IS UNKNOWN;
select timestamp with time zone'2023-01-01 01:01:01 +1' IS NOT TRUE;
select timestamp with time zone'2023-01-01 01:01:01 +1' IS NOT FALSE;
select timestamp with time zone'2023-01-01 01:01:01 +1' IS NOT NULL;
select timestamp with time zone'2023-01-01 01:01:01 +1' IS NOT UNKNOWN;
