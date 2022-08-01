#################################################################
#								#
# Copyright (c) 2020-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TERR028 : OCTO345 : Issue error when inline M function use is attempted

-- Attempt to use an extrinsic function. Use a known existing function to test the case
-- where it is possible to call, but disallowed.
SELECT * FROM names WHERE id = $$ABS^%ydboctosqlfunctions(-2);

-- Attempt to use intrinsic function
SELECT $ZWRITE(firstname) FROM names;

-- Attempt to use inline extrinsic functions with leading % in M label or routine
SELECT * FROM names WHERE id = $$^%("some value");
SELECT * FROM names WHERE id = $$%^%("some value");
SELECT * FROM names WHERE id = $$%ABS^someroutine(-2);
SELECT * FROM names WHERE id = $$ABS^%someroutine(-2);

-- Attempt to use inline extrinsic function in SELECT list
SELECT $$a('b');

-- Attempt to use inline extrinsic function in FROM clause
SELECT * FROM names WHERE $$a('b');

-- Attempt to use inline extrinsic function in HAVING clause
SELECT id FROM names GROUP BY id HAVING $$a(id);

-- Attempt to use inline extrinsic function in alias name
select id as $$dummyextrinsicfunction^something from names;
