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

-- Invalid syntax
select date(horolog)'hhh';
select date(horolog)'hhh.hhh';
select date(horolog)'.';
select date(horolog)',';
select date(horolog)'66644,62290';
select date(horolog)'-66644,62290';
select date(horolog)'+66644,62290';
select date(horolog)'66+644';
select date(horolog)'-';

select time(horolog)'hhh';
select time(horolog)'hhh.hhh';
select time(horolog)'.';
select time(horolog)',';
select time(horolog)'66644,62290';
select time(horolog)'-66644,62290';
select time(horolog)'+66644,62290';
select time(horolog)'66+644';
select time(horolog)'-';
select time(horolog)'+';

select time(horolog) with time zone'hhh';
select time(horolog) with time zone'hhh.hhh';
select time(horolog) with time zone'.';
select time(horolog) with time zone',';
select time(horolog) with time zone'66644,62290';
select time(horolog) with time zone'-66644,62290';
select time(horolog) with time zone'+66644,62290';
select time(horolog) with time zone'66+644';
select time(horolog) with time zone'-';
select time(horolog) with time zone'+';

select timestamp(horolog)'hhh';
select timestamp(horolog)'hhh.hhh';
select timestamp(horolog)'.';
select timestamp(horolog)',';
select timestamp(horolog)'66+644';
select timestamp(horolog)'66753,-';
select timestamp(horolog)'66753,57461,381604,14400';
select timestamp(horolog)'-';
select timestamp(horolog)'+';

select timestamp(horolog) with time zone'hhh';
select timestamp(horolog) with time zone'hhh.hhh';
select timestamp(horolog) with time zone'.';
select timestamp(horolog) with time zone',';
select timestamp(horolog) with time zone'66+644';
select timestamp(horolog) with time zone'66753,-';
select timestamp(horolog) with time zone'66753,57461,381604,14400';
select timestamp(horolog) with time zone'-';
select timestamp(horolog) with time zone'+';

select date(zhorolog)'hhh';
select date(zhorolog)'hhh.hhh';
select date(zhorolog)'.';
select date(zhorolog)',';
select date(zhorolog)'66644,62290';
select date(zhorolog)'-66644,62290';
select date(zhorolog)'+66644,62290';
select date(zhorolog)'66+644';
select date(zhorolog)'-';

select time(zhorolog)'hhh';
select time(zhorolog)'hhh.hhh';
select time(zhorolog)'.';
select time(zhorolog)',';
select time(zhorolog)'66644,62290';
select time(zhorolog)'-66644,62290';
select time(zhorolog)'+66644,62290';
select time(zhorolog)'66+644';
select time(zhorolog)'-';
select time(zhorolog)'+';

select time(zhorolog) with time zone'hhh';
select time(zhorolog) with time zone'hhh.hhh';
select time(zhorolog) with time zone'.';
select time(zhorolog) with time zone',';
--select time(zhorolog) with time zone'66644,62290';
--select time(zhorolog) with time zone'-66644,62290';
--select time(zhorolog) with time zone'+66644,62290';
select time(zhorolog) with time zone'66+644';
select time(zhorolog) with time zone'-';
select time(zhorolog) with time zone'+';

select timestamp(zhorolog)'hhh';
select timestamp(zhorolog)'hhh.hhh';
select timestamp(zhorolog)'.';
select timestamp(zhorolog)',';
select timestamp(zhorolog)'66+644';
select timestamp(zhorolog)'66753,-';
select timestamp(zhorolog)'66753,57461,381604,14400,000';
select timestamp(zhorolog)'-';
select timestamp(zhorolog)'+';

select timestamp(zhorolog) with time zone'hhh';
select timestamp(zhorolog) with time zone'hhh.hhh';
select timestamp(zhorolog) with time zone'.';
select timestamp(zhorolog) with time zone',';
select timestamp(zhorolog) with time zone'66+644';
select timestamp(zhorolog) with time zone'66753,-';
select timestamp(zhorolog) with time zone'66753,57461,381604,14400,000';
select timestamp(zhorolog) with time zone'-';
select timestamp(zhorolog) with time zone'+';

select date(fileman)'hhh';
select date(fileman)'hhh.hhh';
select date(fileman)'.';
select date(fileman)',';
select date(fileman)'2970919.082701';
select date(fileman)'-2970919.082701';
select date(fileman)'+2970919.082701';
select date(fileman)'2970+919';
select date(fileman)'-';

select time(fileman)'hhh';
select time(fileman)'hhh.hhh';
select time(fileman)'.';
select time(fileman)',';
select time(fileman)'2970919.082701';
select time(fileman)'-2970919.082701';
select time(fileman)'+2970919.082701';
select time(fileman)'0827+01';
select time(fileman)'-';
select time(fileman)'+';

select time(fileman) with time zone'hhh';
select time(fileman) with time zone'hhh.hhh';
select time(fileman) with time zone'.';
select time(fileman) with time zone',';
select time(fileman) with time zone'2970919.082701';
select time(fileman) with time zone'-2970919.082701';
select time(fileman) with time zone'+2970919.082701';
select time(fileman) with time zone'0827+01';
select time(fileman) with time zone'-';
select time(fileman) with time zone'+';

select timestamp(fileman)'hhh';
select timestamp(fileman)'hhh.hhh';
select timestamp(fileman)'.';
select timestamp(fileman)',';
select timestamp(fileman)'297+0919';
select timestamp(fileman)'2970919.-';
select timestamp(fileman)'2970919.082701.082701';
select timestamp(fileman)'-';
select timestamp(fileman)'+';

select timestamp(fileman) with time zone'hhh';
select timestamp(fileman) with time zone'hhh.hhh';
select timestamp(fileman) with time zone'.';
select timestamp(fileman) with time zone',';
select timestamp(fileman) with time zone'2970+919';
select timestamp(fileman) with time zone'2970919.-';
select timestamp(fileman) with time zone'2970919.082701.082701';
select timestamp(fileman) with time zone'-';
select timestamp(fileman) with time zone'+';


select date(zut)'hhh';
select date(zut)'hhh.hhh';
select date(zut)'.';
select date(zut)',';
select date(zut)'1696623906640197.082701';
select date(zut)'-1696623906640197,082';
select date(zut)'+1696623906640197';
select date(zut)'1696623906640+197';
select date(zut)'-';

select time(zut)'hhh';
select time(zut)'hhh.hhh';
select time(zut)'.';
select time(zut)',';
select time(zut)'1696623906640197.082701';
select time(zut)'-1696623906640197,082';
select time(zut)'+1696623906640197';
select time(zut)'169662390664+0197';
select time(zut)'-';
select time(zut)'+';

select time(zut) with time zone'hhh';
select time(zut) with time zone'hhh.hhh';
select time(zut) with time zone'.';
select time(zut) with time zone',';
select time(zut) with time zone'1696623906640197.082701';
select time(zut) with time zone'-1696623906640197,082701';
select time(zut) with time zone'+1696623906640197';
select time(zut) with time zone'1696623906640+197';
select time(zut) with time zone'-';
select time(zut) with time zone'+';

select timestamp(zut)'hhh';
select timestamp(zut)'hhh.hhh';
select timestamp(zut)'.';
select timestamp(zut)',';
select timestamp(zut)'1696623906640+197';
select timestamp(zut)'1696623906640197.-';
select timestamp(zut)'1696623906640197.082701';
select timestamp(zut)'-';
select timestamp(zut)'+';

select timestamp(zut) with time zone'hhh';
select timestamp(zut) with time zone'hhh.hhh';
select timestamp(zut) with time zone'.';
select timestamp(zut) with time zone',';
select timestamp(zut) with time zone'169662390664+0197';
select timestamp(zut) with time zone'1696623906640197.-';
select timestamp(zut) with time zone'1696623906640197.082701';
select timestamp(zut) with time zone'-';
select timestamp(zut) with time zone'+';

-- TEXT internal format
select date'hhh';
select date'hhh.hhh';
select date'.';
select date',';
select date'1696623906640197.082701';
select date'-1696623906640197,082';
select date'+1696623906640197';
select date'1696623906640+197';
select date'-';
-- default config set to %m-%d-%Y
select date'01.01.2023';
select date'01/01-2023';
select date'01/01-2023';
select date'01-01-23';
select date'01-01-2023';
select date'01/01/2023';
select date'0a-01-2023';
select date'01-a-2023';
select date'01-01--2023';
select date'01-01-';
select date'';
--

select time'hhh';
select time'hhh.hhh';
select time'.';
select time',';
select time'1696623906640197.082701';
select time'-1696623906640197,082';
select time'+1696623906640197';
select time'169662390664+0197';
select time'-';
select time'+';
-- default config set to %H:%M:%S
select time'1:1:a';
select time'24:01:01';
select time'24:0:0';
select time'23:60:0';
select time'01/00/00';
select time '';
--

select time with time zone'hhh';
select time with time zone'hhh.hhh';
select time with time zone'.';
select time with time zone',';
select time with time zone'1696623906640197.082701';
select time with time zone'-1696623906640197,082701';
select time with time zone'+1696623906640197';
select time with time zone'1696623906640+197';
select time with time zone'-';
select time with time zone'+';

select timestamp'hhh';
select timestamp'hhh.hhh';
select timestamp'.';
select timestamp',';
select timestamp'1696623906640+197';
select timestamp'1696623906640197.-';
select timestamp'1696623906640197.082701';
select timestamp'-';
select timestamp'+';
-- default config set to %m-%d-%Y %H:%M:%S
select timestamp'01.01.2023';
select timestamp'01/01-2023';
select timestamp'01/01-2023';
select timestamp'01-01-23';
select timestamp'01-01-2023';
select timestamp'01/01/2023';
select timestamp'0a-01-2023';
select timestamp'01-a-2023';
select timestamp'01-01--2023';
select timestamp'01-01-';
select timestamp'';
select timestamp'01-01 -2023 00:00:00';
--
select timestamp with time zone'hhh';
select timestamp with time zone'hhh.hhh';
select timestamp with time zone'.';
select timestamp with time zone',';
select timestamp with time zone'169662390664+0197';
select timestamp with time zone'1696623906640197.-';
select timestamp with time zone'1696623906640197.082701';
select timestamp with time zone'-';
select timestamp with time zone'+';

-- Valid syntax but value is having additional data not expected by the type
-- Date should not be present
select time '2023-01-01 11:00:00';
select time without time zone '2023-01-01 11:00:00';

-- Time zone information given to type not expecting time zone
select time '11:00:00 -8:00';
select time without time zone'11:00:00 -8:00';
select timestamp'2023-01-01 11:00:00 -8:00';
select timestamp without time zone'2023-01-01 11:00:00 -8:00';

