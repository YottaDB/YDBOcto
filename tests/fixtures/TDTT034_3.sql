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

select date(fileman)'3230101'::DATE;
select date(fileman)'3230101'::TIMESTAMP;
select date(fileman)'3230101'::TIMESTAMP WITH TIME ZONE;
select date(fileman)'3230101'::VARCHAR;

select time(fileman)'010101'::TIME;
select time(fileman)'010101'::TIME WITH TIME ZONE;
select time(fileman)'010101'::VARCHAR;

select timestamp(fileman)'3230101.010101'::DATE;
select timestamp(fileman)'3230101.010101'::TIME;
select timestamp(fileman)'3230101.010101'::TIMESTAMP;
select timestamp(fileman)'3230101.010101'::TIMESTAMP WITH TIME ZONE;
select timestamp(fileman)'3230101.010101'::VARCHAR;

select CAST(date(fileman)'3230101'AS DATE);
select CAST(date(fileman)'3230101'AS TIMESTAMP);
select CAST(date(fileman)'3230101'AS TIMESTAMP WITH TIME ZONE);
select CAST(date(fileman)'3230101'AS VARCHAR);

select CAST(time(fileman)'010101'AS TIME);
select CAST(time(fileman)'010101'AS TIME WITH TIME ZONE);
select CAST(time(fileman)'010101'AS VARCHAR);

select CAST(timestamp(fileman)'3230101.010101'AS DATE);
select CAST(timestamp(fileman)'3230101.010101'AS TIME);
select CAST(timestamp(fileman)'3230101.010101'AS TIMESTAMP);
select CAST(timestamp(fileman)'3230101.010101'AS TIMESTAMP WITH TIME ZONE);
select CAST(timestamp(fileman)'3230101.010101'AS VARCHAR);

-- More detailed tests
select timestamp(fileman)'3230101.010101'::time < time'01:02:01'; -- Should return `true`
select timestamp(fileman)'3230101.010101'::date < date'2023-01-02'; -- Should return `true`
select (timestamp(fileman)'3230101.010101'::date)::varchar ='2023-01-01'; -- Should return `true`
select (timestamp(fileman)'3230101.010101'::date) ='2023-01-01'::date; -- Should return `true`

select date(fileman)'3230101'::VARCHAR(2);
select date(fileman)'3230101'::VARCHAR(3);
select time(fileman)'010101'::VARCHAR(3);
select timestamp(fileman)'3230101.010101'::VARCHAR(3);

