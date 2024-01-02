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

select date_to_fileman(date'12-31-2699');
select date_to_fileman(date'12-07-2023');
select timestamp_to_fileman(date'12-07-2023'::timestamp);
select date_to_fileman(date(fileman)'9991231');
select date_to_fileman(date(fileman)'3231207');
select timestamp_to_fileman(date(fileman)'3231207'::timestamp);
select timestamp_to_horolog(date(fileman)'9991231'::timestamp);
select timestamp_to_horolog(date(fileman)'3231207'::timestamp);
select timestamp_to_horolog(date(fileman)'9991231'::timestamp)||'hello';
select timestamp_to_horolog(date(fileman)'3231207'::timestamp)||'hello';

select timestamptz_to_zut(timestamp with time zone'01-01-1970 00:00:00+00:00');
select timestamp_to_zut(timestamp '01-01-1970 00:00:00'); -- expect 0
select date_to_zut(date '01-01-1970'); -- expect 0

select date_to_fileman(date'01-01-1701') = 0010101;
select timestamp_to_fileman(date'01-01-1701'::timestamp);
select date_to_fileman(date'01-01-1701');

select timestamp_to_fileman(timestamp(zut) '-27809392000000000');
