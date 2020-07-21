#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TCT014 : OCTO556 : Implement CAST built-in function
-- Heavily modified from `TCT011.sql`

select cast(id as integer),ABS(2) from names;
select cast(id as integer),(cast(ABS(2) as text) || cast(id as text)) from names;
select cast(id as integer),(cast(ABS(2) as numeric) + id) from names;
select cast(id as text) || cast(id as text),cast(ABS(2) as numeric) * id from names;
-- NOTE: Postgres rounds when converting from float to integer, but Octo truncates (SQL Server also truncates, so this is not a bug).
-- So this test case should only use values with a fraction below `.5`.
select id from (select * from names UNION select cast(6.4 as integer),'A','B') subquery;
select id-2*cast(id/2 as integer) from names;
select id-2*cast((id/2) as integer) from names;

-- Test typecast to boolean. Occasionally test that `bool` is equivalent to `boolean`
select cast(id as boolean),(cast(cast(ABS(2) as boolean) as varchar) || cast(id as bool)) from names;
select cast(id as boolean),(cast(ABS(2) as bool) OR cast(id as bool)) from names;
select cast(cast(id as boolean) as text) || cast(cast(id as boolean) as text),cast(ABS(2) as numeric) * id from names;
select id from (select * from names UNION select cast(6.4 as integer),'A','B') subquery;
