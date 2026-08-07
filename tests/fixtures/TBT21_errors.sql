#################################################################
#								#
# Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TBT21 : OCTO1088 : UNKNOWN is only a valid SQL value directly after IS/IS NOT (e.g. "x IS UNKNOWN"), unlike
-- TRUE/FALSE/NULL it is not a general standalone value expression. Below are various positions where a bare
-- UNKNOWN (not preceded by IS/IS NOT) is now correctly rejected with a syntax error.
select unknown;
select unknown from names;
select 1 from names where unknown;
select 1 from names where unknown = unknown;
select 1 from names having unknown;
select coalesce(unknown, true);
select 1 from names order by unknown;
select 1 from names group by unknown;
select true and unknown;
select unknown or false;
select not unknown;

-- Sanity check: TRUE/FALSE/NULL remain valid standalone value expressions (unaffected by this fix)
select true;
select false;
select null;

-- Sanity check: IS UNKNOWN / IS NOT UNKNOWN continue to work exactly as before (unaffected by this fix)
select * from names where (firstname = 'Zero') is unknown;
select * from names where (firstname = 'Zero') is not unknown;

-- Sanity check: UNKNOWN still works fine as an identifier, e.g. a column name (unaffected by this fix --
-- this goes through a separate grammar path, "sql_identifier_exceptions", not "boolean_primary")
create table TBT21 (unknown int);
drop table TBT21;
