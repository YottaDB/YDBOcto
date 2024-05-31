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

select timestamp with time zone '03-25-1868 08:33:14-12:45' as timestamptz_;
-- timestamptz_
-- 1868-03-25 16:22:12-04:56:02
-- (1 row)
select timestamptz_to_fileman(timestamp with time zone '03-25-1868 08:33:14-12:45');
-- timestamptz_to_fileman
-- 1680325.162212
-- (1 row)
select timestamptz_to_horolog(timestamp with time zone '03-25-1868 08:33:14-12:45');
-- timestamptz_to_horolog
-- 9946,58932
-- (1 row)
select timestamptz_to_zhorolog(timestamp with time zone '03-25-1868 08:33:14-12:45');
-- timestamptz_to_zhorolog
-- 9946,58932,,17760
-- (1 row)
set datestyle="ymd";
select timestamp with time zone '1868-03-25 16:22:12-04:56:02' = timestamp(fileman) with time zone '1680325.162212';
-- ???
-- 1
-- (1 row)
select timestamp with time zone '1868-03-25 16:22:12-04:56:02' = timestamp(horolog) with time zone '9946,58932';
-- ???
-- 1
-- (1 row)
select timestamp with time zone '1868-03-25 16:22:12-04:56' = timestamp(zhorolog) with time zone '9946,58932,,17760';
-- ???
-- 1
-- (1 row)
select timestamp with time zone '1868-03-25 16:22:12-04:56' = timestamp(zhorolog) with time zone '9946,58932,,17762';
-- ???
-- 0
-- (1 row)
select timestamp with time zone '1868-03-25 16:22:12-04:56';
-- ???
-- 1868-03-25 16:22:10-04:56:02
-- (1 row)
select timestamptz_to_fileman(timestamp with time zone '1868-03-25 16:22:12-04:56');
-- timestamptz_to_fileman
-- 1680325.162210
-- (1 row)
select timestamp with time zone '1868-03-25 16:22:12-04:56' = timestamp(fileman) with time zone '1680325.162210';
-- ???
-- 1
-- (1 row)

