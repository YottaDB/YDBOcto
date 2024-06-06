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

-- 19 is used inorder to avoid microsecond
select now()::timestamp + time'01:01:01';
select now()::timestamp - time'01:01:01';

select localtimestamp::timestamp + time'01:01:01';
select localtimestamp::timestamp - time'01:01:01';

select current_timestamp::timestamp + time'01:01:01';
select current_timestamp::timestamp - time'01:01:01';

-- 8 is used to avoid microsecond and second values
select localtime;

-- 8 is used to avoid microsecond and second values
select current_time;
