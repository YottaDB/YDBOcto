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

-- select now(); -- microseconds will be different so cannot compare
-- select now()::time; -- microseconds will be different so cannot compare
-- select now()::timestamp; -- microseconds will be different so cannot compare
-- select now()::timestamp with time zone; -- microseconds will be different so cannot compare
-- now()
select now()::date;
select now() between now()::timestamp - time'01:01:01' and now()::timestamp + time'01:01:01';

-- localtimestamp
select localtimestamp::date;
select localtimestamp between localtimestamp::timestamp - time'01:01:01' and localtimestamp::timestamp + time'01:01:01';
-- current_timestamp
select current_timestamp::date;
select current_timestamp between current_timestamp::timestamp - time'01:01:01' and current_timestamp::timestamp + time'01:01:01';
-- localtime
select localtime::time between (now()::timestamp - time'01:01:01')::time and (now()::timestamp + time'01:01:01')::time;
-- current_time
select current_time between (now()::timestamp - time'01:01:01')::time and (now()::timestamp + time'01:01:01')::time;
