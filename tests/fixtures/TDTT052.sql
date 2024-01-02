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
-- 17 is used inorder to avoid microsecond and second values as they might change easily when switching from octo to postgres
-- now()
select now()::date;
select (now()::timestamp + time'01:01:01')::VARCHAR(17);
select (now()::timestamp - time'01:01:01')::VARCHAR(17);
select now() between now()::timestamp - time'01:01:01' and now()::timestamp + time'01:01:01';

-- localtimestamp
select localtimestamp::date;
select (localtimestamp::timestamp + time'01:01:01')::VARCHAR(17);
select (localtimestamp::timestamp - time'01:01:01')::VARCHAR(17);
select localtimestamp between localtimestamp::timestamp - time'01:01:01' and localtimestamp::timestamp + time'01:01:01';
-- current_timestamp
select current_timestamp::date;
select (current_timestamp::timestamp + time'01:01:01')::VARCHAR(17);
select (current_timestamp::timestamp - time'01:01:01')::VARCHAR(17);
select current_timestamp between current_timestamp::timestamp - time'01:01:01' and current_timestamp::timestamp + time'01:01:01';
-- localtime
-- 6 is used to avoid microsecond and second values
select (localtime)::varchar(6);
select localtime::time between (now()::timestamp - time'01:01:01')::time and (now()::timestamp + time'01:01:01')::time;
-- current_time
-- 6 is used to avoid microsecond and second values
select (current_time)::varchar(6);
select current_time between (now()::timestamp - time'01:01:01')::time and (now()::timestamp + time'01:01:01')::time;
