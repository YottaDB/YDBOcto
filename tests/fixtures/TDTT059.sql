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

select date_to_fileman(date'2699-12-31');
select date_to_fileman(date'2023-12-07');
select timestamp_to_fileman(date'2023-12-07'::timestamp);
select date_to_fileman(date(fileman)'9991231');
select date_to_fileman(date(fileman)'3231207');
select timestamp_to_fileman(date(fileman)'3231207'::timestamp);
select timestamp_to_horolog(date(fileman)'9991231'::timestamp);
select timestamp_to_horolog(date(fileman)'3231207'::timestamp);
select timestamp_to_horolog(date(fileman)'9991231'::timestamp)||'hello';
select timestamp_to_horolog(date(fileman)'3231207'::timestamp)||'hello';

select timestamptz_to_zut(timestamp with time zone'1970-01-01 00:00:00+00:00');
select timestamp_to_zut(timestamp '1970-01-01 00:00:00'); -- expect 0
select date_to_zut(date '1970-01-01'); -- expect 0

select date_to_fileman(date'1701-01-01') = 0010101;
select timestamp_to_fileman(date'1701-01-01'::timestamp);
select date_to_fileman(date'1701-01-01');

select timestamp_to_fileman(timestamp(zut) '-27809392000000000');
