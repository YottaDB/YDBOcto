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

select date(horolog)'66475'::DATE;
select date(horolog)'66475'::TIMESTAMP;
select date(horolog)'66475'::TIMESTAMP WITH TIME ZONE;
select date(horolog)'66475'::VARCHAR;

select time(horolog)'3661'::TIME;
-- select time(horolog)'3661'::TIME WITH TIME ZONE;
select time(horolog)'3661'::VARCHAR;

select timestamp(horolog)'66475,3661'::DATE;
select timestamp(horolog)'66475,3661'::TIME;
select timestamp(horolog)'66475,3661'::TIMESTAMP;
select timestamp(horolog)'66475,3661'::TIMESTAMP WITH TIME ZONE;
select timestamp(horolog)'66475,3661'::VARCHAR;

select CAST(date(horolog)'66475'AS DATE);
select CAST(date(horolog)'66475'AS TIMESTAMP);
select CAST(date(horolog)'66475'AS TIMESTAMP WITH TIME ZONE);
select CAST(date(horolog)'66475'AS VARCHAR);

select CAST(time(horolog)'3661'AS TIME);
-- select CAST(time(horolog)'3661'AS TIME WITH TIME ZONE);
select CAST(time(horolog)'3661'AS VARCHAR);

select CAST(timestamp(horolog)'66475,3661'AS DATE);
select CAST(timestamp(horolog)'66475,3661'AS TIME);
select CAST(timestamp(horolog)'66475,3661'AS TIMESTAMP);
select CAST(timestamp(horolog)'66475,3661'AS TIMESTAMP WITH TIME ZONE);
select CAST(timestamp(horolog)'66475,3661'AS VARCHAR);

-- More detailed tests
select timestamp(horolog)'66475,3661'::time < time'01:02:01'; -- Should return `true`
select timestamp(horolog)'66475,3661'::date < date'2023-01-02'; -- Should return `true`
select (timestamp(horolog)'66475,3661'::date)::varchar ='2023-01-01'; -- Should return `true`
select (timestamp(horolog)'66475,3661'::date) ='2023-01-01'::date; -- Should return `true`

select date(horolog)'66475'::VARCHAR(2);
select date(horolog)'66475'::VARCHAR(3);
select time(horolog)'3661'::VARCHAR(3);
select timestamp(horolog)'66475,3661'::VARCHAR(3);

