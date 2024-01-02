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

-- samevalue() returns the value as received but when a function is said to return date/time
-- type value in text format the value is expected to be in output form set in datestyle (ISO - "YMD").
create function samevalue(date) returns date as $$samevalue^functions;
-- Because the return value is not in YMD format following selects result in ERROR
select samevalue(date'01-01-2023');
select samevalue(samevalue(date'01-01-2023'));
set datestyle='ymd';
-- As the implementation returns the input passed as is following selects work fine as the input
-- is also in YMD format.
select samevalue(samevalue(date'2023-01-01'));
select samevalue(samevalue(samevalue(date'2023-01-01')));
drop function samevalue(date);
set datestyle='mdy';
create function samevalue(timestamp) returns timestamp as $$samevalue^functions;
-- ERROR because of the reason explained for date type
select samevalue(timestamp'01-01-2023 01:01:00');
select samevalue(samevalue(timestamp'01-01-2023 01:01:00'));
set datestyle='ymd';
-- Valid because of the same reason explained for date type
select samevalue(samevalue(timestamp'2023-01-01 01:01:00'));
select samevalue(samevalue(samevalue(timestamp'2023-01-01 01:01:00')));
drop function samevalue(timestamp);
-- The function returns YMD for output
create function samevalue(date) returns date as $$samevaluemdy^functions;
-- Valid as the result is in YMD form
set datestyle='mdy';
select samevalue(date'01-01-2023');
select samevalue(samevalue(date'01-01-2023'));
select samevalue(samevalue(samevalue(date'01-01-2023')));
-- sql functions
set datestyle='mdy';
select date_to_fileman(now()::date)=date_to_fileman(now()::date);
select timestamp_to_fileman(now()::timestamp)=timestamp_to_fileman(now()::timestamp);
set datestyle='ymd';
select date_to_fileman(now()::date)=date_to_fileman(now()::date);
select timestamp_to_fileman(now()::timestamp)=timestamp_to_fileman(now()::timestamp);
