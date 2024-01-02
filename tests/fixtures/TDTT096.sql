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

select timestamp with time zone'01-01-0222 01:01:01';
select timestamp with time zone'01-01-0222 01:01:01' || 'same ple';
select 'sameple asd' || timestamp with time zone'01-01-0222 01:01:01' || 'samep pl';
select timestamp with time zone'1-2-0830 3:19:2.74562-05:37';
