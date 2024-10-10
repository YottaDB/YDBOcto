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

-- TAF07 : OCTO617 : Test MAX(COL) and MIN(COL) is optimized or not in various cases where COL is a non-key-column

-- First check queries that we expect to be optimized
select max(firstname) from names;
select min(lastname) from names;
select 'abcd' || min(lastname) from names;
select 'abcd' || min(n1.firstname), 'efgh' || max(n1.lastname), max(n1.firstname) || min(n1.lastname) from names n1;
-- Test max/min on all columns of names table. Also test that min(lastname) does not return NULL.
select max(id), min(id), max(firstname), min(firstname), max(lastname), min(lastname) from names;

-- Then check queries that we don't expect to be optimized

-- Query with JOIN
select max(n1.firstname) from names n1, names n2;
-- Query with WHERE
select max(lastname) from names where lastname < 'qrst';
-- Query with GROUP BY
select max(firstname) from names group by firstname;
select max(lastname) from names group by firstname;
-- Query with HAVING
select max(firstname) from names having min(firstname) > 'mnop';
-- Query with NO aggregate function
select firstname from names;
-- Query with aggregate functions that are not simple column references or literals
select min(firstname || 'abcd') from names;
select min(3*2) from names;
select 1 from names order by max(lastname || 'ijkl');

-- Additionally verify that ORDER BY clause is NOT removed even if YDBOcto#617 optimization can be applied by
-- verifying that in the emitted M plan there is a FOR loop for the ORDER BY in this case
-- See https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/617#note_2151071683 for why ORDER BY is NOT removed
select min(firstname) from names order by max(lastname);
select min(2) from names order by max(lastname);
select min(firstname) from names order by max(2);

