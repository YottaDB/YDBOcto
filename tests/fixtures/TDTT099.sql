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

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1404#note_1875565109
select * from names where firstname = (select firstname from names order by firstname limit 1);

-- New format checks
select timestamp'2023-01-01T01:01:01'=timestamp'2023-01-01 01:01:01';
select timestamp'2023-01-01T01:01:01'<date'2023-01-01';
select timestamp'2023-01-01T01:01:01'<date'2023-01-02';
select timestamp'2023-01-01T01:01:01'>date'2023-01-01';

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/1055
select date'2023-08-01'::timestamp with time zone;
select timestamp with time zone'2023-08-01';
select timestamp with time zone'2023-08-01' = date'2023-08-01'::timestamp with time zone;
select timestamp'2023-08-01 00:00:00'::timestamp with time zone;
select timestamp with time zone'2023-08-01 00:00:00' = timestamp'2023-08-01 00:00:00'::timestamp with time zone;
