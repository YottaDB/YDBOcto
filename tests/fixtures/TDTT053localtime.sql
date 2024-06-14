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

-- localtime -- returns timestamp in MYSQL but Octo returns time similar to Postgres, hence the cast below
select cast(localtime as time);
-- select localtime between localtime - time'01:01:01' and localtime + time'01:01:01';
select now();
select localtimestamp;
select current_timestamp;
