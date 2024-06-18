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

select date'2023-01-01'::DATE;
select date'2023-01-01'::TIMESTAMP;
select date'2023-01-01'::TIMESTAMP WITH TIME ZONE;
select date'2023-01-01'::VARCHAR;

select time'01:01:01'::TIME;
--select time'01:01:01'::TIME WITH TIME ZONE;
select time'01:01:01'::VARCHAR;

select time with time zone'01:01:01 -5'::TIME;
--select time with time zone'01:01:01 -5'::TIME WITH TIME ZONE;
select time with time zone'01:01:01 -5'::VARCHAR;

select timestamp'2023-01-01 01:01:01'::DATE;
select timestamp'2023-01-01 01:01:01'::TIME;
select timestamp'2023-01-01 01:01:01'::TIMESTAMP;
select timestamp'2023-01-01 01:01:01'::TIMESTAMP WITH TIME ZONE;
select timestamp'2023-01-01 01:01:01'::VARCHAR;

select timestamp with time zone'2023-01-01 01:01:01 -5'::DATE;
select timestamp with time zone'2023-01-01 01:01:01 -5'::TIME;
select timestamp with time zone'2023-01-01 01:01:01 -5'::TIMESTAMP;
select timestamp with time zone'2023-01-01 01:01:01 -5'::TIMESTAMP WITH TIME ZONE;
select timestamp with time zone'2023-01-01 01:01:01 -5'::VARCHAR;

select CAST(date'2023-01-01' AS DATE);
select CAST(date'2023-01-01' AS TIMESTAMP);
select CAST(date'2023-01-01' AS TIMESTAMP WITH TIME ZONE);
select CAST(date'2023-01-01' AS VARCHAR);

select CAST(time'01:01:01' AS TIME);
--select CAST(time'01:01:01' AS TIME WITH TIME ZONE);
select CAST(time'01:01:01' AS VARCHAR);

select CAST(time with time zone'01:01:01 -5' AS TIME);
--select CAST(time with time zone'01:01:01 -5' AS TIME WITH TIME ZONE);
select CAST(time with time zone'01:01:01 -5' AS VARCHAR);

select CAST(timestamp'2023-01-01 01:01:01' AS DATE);
select CAST(timestamp'2023-01-01 01:01:01' AS TIME);
select CAST(timestamp'2023-01-01 01:01:01' AS TIMESTAMP);
select CAST(timestamp'2023-01-01 01:01:01' AS TIMESTAMP WITH TIME ZONE);
select CAST(timestamp'2023-01-01 01:01:01' AS VARCHAR);

select CAST(timestamp with time zone'2023-01-01 01:01:01 -5' AS DATE);
select CAST(timestamp with time zone'2023-01-01 01:01:01 -5' AS TIME);
select CAST(timestamp with time zone'2023-01-01 01:01:01 -5' AS TIMESTAMP);
select CAST(timestamp with time zone'2023-01-01 01:01:01 -5' AS TIMESTAMP WITH TIME ZONE);
select CAST(timestamp with time zone'2023-01-01 01:01:01 -5' AS VARCHAR);

-- More detailed tests
select timestamp'2023-01-01 01:01:01'::time < time'01:02:01'; -- Should return `true`
select timestamp'2023-01-01 01:01:01'::date < date'2023-01-02'; -- Should return `true`
select (timestamp'2023-01-01 01:01:01'::date)::varchar ='2023-01-01'; -- Should return `true` for text output format
select (timestamp'2023-01-01 01:01:01'::date)::varchar ='66475'; -- Should return `true` for horolog output format
select (timestamp'2023-01-01 01:01:01'::date)::varchar ='3230101'; -- Should return `true` for fileman output format
select (timestamp'2023-01-01 01:01:01'::date) ='2023-01-01'::date; -- Should return `true`

select '2023-01-01'::date;
select '2023-01-01 01:01:01'::date;
select '01:01:01'::time;
select '2023-01-01 01:01:01'::timestamp;

select date'2023-01-01'::VARCHAR(2);
select date'2023-01-01'::VARCHAR(3);
select time'01:01:01'::VARCHAR(3);
select timestamp'2023-01-01 01:01:01'::VARCHAR(3);

