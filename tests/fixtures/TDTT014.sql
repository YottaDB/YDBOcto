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

-- IN
select date'01-01-2023' IN (date'01-02-2023', date'01-03-2023', date'01-01-2023');
select time'01:01:01' IN (time'01:01:20', time'01:03:23', time'01:01:01');
select timestamp'01-01-2023 01:01:01' IN (timestamp'01-02-2023 01:01:01', timestamp'01-01-2023 01:02:01',timestamp'01-01-2023 01:01:01');
-- -- subquery result based
select date'01-01-2023' IN (select date'01-02-2023');
select time'01:01:01' IN (select time'01:01:20');
select timestamp'01-01-2023 01:01:01' IN (select timestamp'01-02-2023 01:01:01');
-- NOT IN
select date'01-01-2023' NOT IN (date'01-02-2023', date'01-03-2023', date'01-01-2023');
select time'01:01:01' NOT IN (time'01:01:20', time'01:03:23', time'01:01:01');
select timestamp'01-01-2023 01:01:01' NOT IN (timestamp'01-02-2023 01:01:01', timestamp'01-01-2023 01:02:01',timestamp'01-01-2023 01:01:01');
-- -- subquery result based
select date'01-01-2023' NOT IN (select date'01-01-2023');
select time'01:01:01' NOT IN (select time'01:01:20');
select timestamp'01-01-2023 01:01:01' NOT IN (select timestamp'01-02-2023 01:01:01');
