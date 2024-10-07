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

-- TAF06 : OCTO617 : Test MAX(COL) and MIN(COL) is optimized in various cases and not in various other cases

-- First check queries that we expect to be optimized
select max(id) from names;
select min(id) from names;
select 5 + min(id) from names;
select 5 + min(n1.id), 10 + max(n1.id), max(n1.id) - min(n1.id) from names n1;

-- Then check queries that we don't expect to be optimized

-- Query with JOIN
select max(n1.id) from names n1, names n2;
-- Query with WHERE
select max(id) from names where id < 4;
-- Query with GROUP BY
select max(id) from names group by id;
select max(id) from names group by firstname;
-- Query with HAVING
select max(id) from names having min(id) > 6;
-- Query with NO aggregate function
select firstname from names;
-- Query with aggregate function usage that is not MIN or MAX
select sum(id) from names;
-- Query with aggregate functions that are not simple column references or literals
select min(id+2) from names;
select min(3*2) from names;
select 1 from names order by max(id+2);

-- Additionally verify that ORDER BY clause is NOT removed even if YDBOcto#617 optimization can be applied by
-- verifying that in the emitted M plan there is a FOR loop for the ORDER BY in this case
-- See https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/617#note_2151071683 for why ORDER BY is NOT removed
select min(id) from names order by max(id);
select min(2) from names order by max(id);
select min(id) from names order by max(2);

