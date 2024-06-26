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

select CASE date'2023-01-01' WHEN date'2023-01-01' THEN date'2023-01-02' ELSE date'2023-01-03' END;
select CASE WHEN date'2023-01-01' = date'2023-01-01' THEN date'2023-01-02' ELSE date'2023-01-03' END;

select CASE time'01:01:20' WHEN time'01:01:20' THEN time'01:02:20' ELSE time'01:03:20' END;
select CASE WHEN time'01:01:20' = time'01:01:20' THEN time'01:02:20' ELSE time'01:03:20' END;

select CASE timestamp'2023-01-01 00:00:00' WHEN timestamp'2023-01-01 00:00:00' THEN timestamp'2023-01-02 00:00:00' ELSE timestamp'2023-01-03 00:00:00' END;
select CASE WHEN timestamp'2023-01-01 00:00:00' = timestamp'2023-01-01 00:00:00' THEN timestamp'2023-01-02 00:00:00' ELSE timestamp'2023-01-03 00:00:00' END;
