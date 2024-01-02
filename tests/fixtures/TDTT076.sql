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

select timestamp(zhorolog) with time zone '2980013,86399,999999,-50399';
select timestamp(zhorolog) with time zone '2980013,86399,999999,-50400';
select timestamp(zhorolog) with time zone '2980013,86399,999999,-50401';
-- [ERROR]: ERR_INVALID_DATE_TIME_FORMAT: Invalid date or time format 2980013,86399,999999,-50401
select timestamp(zhorolog) with time zone '2980013,86399,999999,-50340';
select timestamp(zhorolog) with time zone '2980013,86399,999999,-50339';
select timestamp(zhorolog) with time zone '2980013,86399,999999,-50338';
select timestamp(zhorolog) with time zone '-365,0,,43201';
-- [ERROR]: ERR_INVALID_DATE_TIME_FORMAT: Invalid date or time format -365,0,,43201
select timestamp(zhorolog) with time zone '-365,0,,43200';
select timestamp(zhorolog) with time zone '-365,0,,43199';
select timestamp(zhorolog) with time zone '-365,0,,43198';
select timestamp(horolog) '228893,65111' <= timestamp(zhorolog) with time zone '194240,17291,+43662,22739'; -- 0
