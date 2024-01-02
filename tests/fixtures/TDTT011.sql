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

select nullif(date'01-02-2023',timestamp with time zone'01-01-2023 00:00:00');
select nullif(timestamp with time zone'01-01-2023 00:00:00',date'01-02-2023');
select nullif(timestamp with time zone'01-01-2023 00:00:00',date'01-01-2023');
select nullif(date'01-02-2023',timestamp'01-01-2023 00:00:00');
select nullif(timestamp'01-01-2023 00:00:00',date'01-02-2023');
select nullif(timestamp'01-01-2023 00:00:00',date'01-01-2023');

select COALESCE(date'01-01-2023','01-01-2023');
select GREATEST(date'01-01-2023','01-01-2023');
select LEAST(date'01-01-2023','01-01-2023');
select NULLIF(date'01-01-2023','01-01-2023');
select COALESCE(time'01:01:00','01:01:01');
select GREATEST(time'01:01:00','01:01:01');
select LEAST(time'01:01:00','01:01:01');
select NULLIF(time'01:01:00','01:01:01');
select COALESCE(timestamp'01-01-2023 01:01:00','01-01-2023 01:01:01');
select GREATEST(timestamp'01-01-2023 01:01:00','01-01-2023 01:01:01');
select LEAST(timestamp'01-01-2023 01:01:00','01-01-2023 01:01:01');
select NULLIF(timestamp'01-01-2023 01:01:00','01-01-2023 01:01:01');
select COALESCE(timestamp with time zone'01-01-2023 01:01:00-05:00','01-01-2023 01:01:01-05');
select GREATEST(timestamp with time zone'01-01-2023 01:01:00-05:00','01-01-2023 01:01:01-05');
select LEAST(timestamp with time zone'01-01-2023 01:01:00-05:00','01-01-2023 01:01:01-05');
select NULLIF(timestamp with time zone'01-01-2023 01:01:00-05:00','01-01-2023 01:01:01-05');
