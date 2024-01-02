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

select date'01-01-2023'+time'01:01:01';
select date'01-01-2023'+1;
select date(fileman)'3230101'+time'01:01:01';
select timestamp'01-01-2023 01:01:01'+time'01:01:01';
select timestamp'01-01-2023 01:01:01'+time'23:59:59';
