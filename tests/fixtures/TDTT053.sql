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

--select current_timestamp; -- YYYY-MM-DD hh:mm:ss no micro second or time zone
--select localtime(); -- YYYY-MM-DD hh:mm:ss no micro second or time zone
--select localtimestamp(); -- YYYY-MM-DD hh:mm:ss no micro second or time zone
--select now(); -- YYYY-MM-DD hh:mm:ss no micro second or time zone

-- COmmented tests below will not work as mysql has a return type of now and other functions are timestamp in mysql where
-- as in Postgres and Octo it is timestamp with time zone. During MYSQL emulation the result seen will be a timestamp but
-- it is declared as a timestamp with timezone in Octo
select date_format(now(),'%m-%d-%Y');
select CAST(now() as date);
select CAST(now() as char(17));
--select now() between now() - time'01:01:01' and now() + time'01:01:01';

-- localtimestamp
select cast(localtimestamp as date);
select cast(localtimestamp as char(17));
--select localtimestamp between localtimestamp - time'01:01:01' and localtimestamp + time'01:01:01';
-- current_timestamp
select cast(current_timestamp as date);
select cast(current_timestamp as char(17));
--select current_timestamp between current_timestamp - time'01:01:01' and current_timestamp + time'01:01:01';
-- localtime -- returns timestamp in MYSQL but Octo returns time similar to Postgres, hence the cast below
select cast(localtime as time);
--select localtime between localtime - time'01:01:01' and localtime + time'01:01:01';
