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
select (date '2023-11-29' - date '2023-11-28' + date '2023-11-28');
select date '2023-11-29' + 1 +2;
select 1 + 2 + date'2023-11-29';
select 2 + date'2023-11-29' + 1;
select 2 + date'2023-11-29' + 1 - (date'2023-11-29'+3);
select date'2023-11-29'+2 <= (date'2023-11-29'+3)::timestamp;

select date'2023-01-01'-time'01:01:01' = timestamp'2022-12-31 22:58:59';
select date'2023-01-01'-time'01:01:01'-time'01:01:01';

select date'2023-01-01'- -5 - time'01:01:01';
select (select (select now()))::date=now()::date;
select now()::date=now()::date;
-- select now()=now() -- This fails in Octo where as it doesn't in Postgres, The sub second information is different between first and second invocation in Octo.
select time'01:01:01'||'sample'||timestamp'2023-01-01 01:01:01'||date'2024-01-01';

select date '2023-11-29' - null::integer;
select date '2023-11-29' + null::integer;
select (date '2023-11-29' + (date '2023-11-28' - date '2023-11-28'));
