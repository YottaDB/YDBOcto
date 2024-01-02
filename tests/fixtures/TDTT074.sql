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
-- ERR_TYPE_NOT_COMPATIBLE
select (date '11-29-2023' + date '11-28-2023' - date '11-28-2023');

-- ERR_INVALID_DATE_TIME_VALUE
select date(zut) '-27809392000000000';
select date_to_fileman(date(zut) '-27809392000000000');
select date_to_horolog(date(zut) '-27809392000000000');
select date_to_zhorolog(date(zut) '-27809392000000000');
select timestamp with time zone'6-12-5950 13:12:66.59647+00:39' - time'5:8:0';
select time(zhorolog) with time zone ',25210,078868,23372299286180000000001';
select date'01-01-2023 .-01-2023';
select '01-01-2023 .-01-2023'::date;
select '01-01-2023 .1:0final,first,float,floor,following,forrespondjng,count,covar_pop,cova1:01'::date;

-- ERR_DATE_TIME_RESULT_OUT_OF_RANGE
select date'01-01-2023' + 660000001-2-23;
select date'01-01-2023' - 660000001;
select date'01-01-2024'-date'01-02-2024'; -- This should not error. This ensure a -1 return doesn't flag an error
select date'01-01-2023' + 3230101784-2023;

-- Size cannot be specified for date/time type
create table tmpd (id integer primary key, o_date date(1));
create table tmpd (id integer primary key, o_date time(1));
create table tmpd (id integer primary key, o_date timestamp(1));
create table tmpd (id integer primary key, o_date timestamp with time zone(1));
create table tmpd (id integer primary key, o_date time with time zone(1));
