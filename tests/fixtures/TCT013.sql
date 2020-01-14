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

-- TCT013 : OCTO235 : Rename INT data type to NUMERIC as we do also support floating-point/decimal numbers

SELECT id + firstname FROM names;
SELECT id - firstname FROM names;
SELECT id * firstname FROM names;
SELECT id / firstname FROM names;
SELECT id % firstname FROM names;
SELECT id || firstname FROM names;
SELECT firstname + lastname FROM names;
SELECT firstname - lastname FROM names;
SELECT firstname * lastname FROM names;
SELECT firstname / lastname FROM names;
SELECT firstname % lastname FROM names;
SELECT firstname || lastname FROM names;

-- Test typecast operator for various supported types (INTEGER/NUMERIC/VARCHAR/TEXT) and invalid type

-- Test typecast of integer literals
SELECT 1::integer FROM names;
SELECT 1::numeric FROM names;
SELECT 1::varchar FROM names;
SELECT 1::text FROM names;
SELECT 1::invalid FROM names;

-- Test typecast of numeric literals
SELECT -1.5::integer FROM names;
SELECT -1.5::numeric FROM names;
SELECT -1.5::varchar FROM names;
SELECT -1.5::text FROM names;
SELECT -1.5::invalid FROM names;

SELECT (-1.5)::integer FROM names;
SELECT (-1.5)::numeric FROM names;
SELECT (-1.5)::varchar FROM names;
SELECT (-1.5)::text FROM names;
SELECT (-1.5)::invalid FROM names;

-- Test typecast of string literals
SELECT 'abcd'::integer FROM names;
SELECT 'abcd'::numeric FROM names;
SELECT 'abcd'::varchar FROM names;
SELECT 'abcd'::text FROM names;
SELECT 'abcd'::invalid FROM names;

SELECT '1.5abcd'::integer FROM names;
SELECT '2.5abcd'::numeric FROM names;
SELECT '3.5abcd'::varchar FROM names;
SELECT '4.5abcd'::text FROM names;
SELECT '5.5abcd'::invalid FROM names;

-- Test typecast of integer column references
SELECT id::integer FROM names;
SELECT id::numeric FROM names;
SELECT id::varchar FROM names;
SELECT id::text FROM names;
SELECT id::invalid FROM names;

-- Test typecast of numeric column references
select Price::integer from Products;
select Price::numeric from Products;
select Price::varchar from Products;
select Price::text from Products;
select Price::invalid from Products;

-- Test typecast of string column references
SELECT firstname::integer FROM names;
SELECT firstname::numeric FROM names;
SELECT firstname::varchar FROM names;
SELECT firstname::text FROM names;
SELECT firstname::invalid FROM names;

