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

-- TL08 : OCTO433 : LIMIT in sub-query causes LIMIT in outer query with OR/IN operator in WHERE clause to return more rows than expected
--
-- Note: There are 4 Sections below. Only Section 2 demonstrates issue #433.
--	The other sections are added for completeness of testing all combinations.
--	The `-- rowcount-only-check` comment is added to Sections 1,2,3 because when LIMIT is used without ORDER BY we cannot check
--	actual output of Octo against Postgres output. Can only count that the # of rows is the same. Section 1 and Section 3 use
--	LIMIT without ORDER BY in the sub-query whereas Section 2 uses LIMIT without ORDER BY in the outer query. Only Section 4
--	uses LIMIT with ORDER BY in both sub-query and outer query.

-- Section 1
SELECT (SELECT n1.id FROM names n1 LIMIT 1),firstname FROM names WHERE id IN (1,2)               LIMIT 1; -- rowcount-only-check
SELECT (SELECT n1.id FROM names n1 LIMIT 1),firstname FROM names WHERE id IN (1,2,3)             LIMIT 1; -- rowcount-only-check
SELECT (SELECT n1.id FROM names n1 LIMIT 1),firstname FROM names WHERE id IN (1,2,3,4)           LIMIT 1; -- rowcount-only-check
SELECT (SELECT n1.id FROM names n1 LIMIT 1),firstname FROM names WHERE id IN (1,2,3,4,5)         LIMIT 1; -- rowcount-only-check
SELECT (SELECT n1.id FROM names n1 LIMIT 1),firstname FROM names WHERE id IN (1,2,3,4,5,0)       LIMIT 1; -- rowcount-only-check
SELECT (SELECT n1.id FROM names n1 LIMIT 1),firstname FROM names WHERE id IN (1,2,3,4,5,0,1,2,3) LIMIT 1; -- rowcount-only-check

-- Section 2
SELECT (SELECT n1.id FROM names n1 ORDER BY n1.id LIMIT 1),firstname FROM names WHERE id IN (1,2)               LIMIT 1; -- rowcount-only-check
SELECT (SELECT n1.id FROM names n1 ORDER BY n1.id LIMIT 1),firstname FROM names WHERE id IN (1,2,3)             LIMIT 1; -- rowcount-only-check
SELECT (SELECT n1.id FROM names n1 ORDER BY n1.id LIMIT 1),firstname FROM names WHERE id IN (1,2,3,4)           LIMIT 1; -- rowcount-only-check
SELECT (SELECT n1.id FROM names n1 ORDER BY n1.id LIMIT 1),firstname FROM names WHERE id IN (1,2,3,4,5)         LIMIT 1; -- rowcount-only-check
SELECT (SELECT n1.id FROM names n1 ORDER BY n1.id LIMIT 1),firstname FROM names WHERE id IN (1,2,3,4,5,0)       LIMIT 1; -- rowcount-only-check
SELECT (SELECT n1.id FROM names n1 ORDER BY n1.id LIMIT 1),firstname FROM names WHERE id IN (1,2,3,4,5,0,1,2,3) LIMIT 1; -- rowcount-only-check

-- Section 3
SELECT (SELECT n1.id FROM names n1 LIMIT 1),firstname FROM names WHERE id IN (1,2)               ORDER BY 2 LIMIT 1; -- rowcount-only-check
SELECT (SELECT n1.id FROM names n1 LIMIT 1),firstname FROM names WHERE id IN (1,2,3)             ORDER BY 2 LIMIT 1; -- rowcount-only-check
SELECT (SELECT n1.id FROM names n1 LIMIT 1),firstname FROM names WHERE id IN (1,2,3,4)           ORDER BY 2 LIMIT 1; -- rowcount-only-check
SELECT (SELECT n1.id FROM names n1 LIMIT 1),firstname FROM names WHERE id IN (1,2,3,4,5)         ORDER BY 2 LIMIT 1; -- rowcount-only-check
SELECT (SELECT n1.id FROM names n1 LIMIT 1),firstname FROM names WHERE id IN (1,2,3,4,5,0)       ORDER BY 2 LIMIT 1; -- rowcount-only-check
SELECT (SELECT n1.id FROM names n1 LIMIT 1),firstname FROM names WHERE id IN (1,2,3,4,5,0,1,2,3) ORDER BY 2 LIMIT 1; -- rowcount-only-check

-- Section 4
SELECT (SELECT n1.id FROM names n1 ORDER BY n1.id LIMIT 1),firstname FROM names WHERE id IN (1,2)               ORDER BY 2 LIMIT 1;
SELECT (SELECT n1.id FROM names n1 ORDER BY n1.id LIMIT 1),firstname FROM names WHERE id IN (1,2,3)             ORDER BY 2 LIMIT 1;
SELECT (SELECT n1.id FROM names n1 ORDER BY n1.id LIMIT 1),firstname FROM names WHERE id IN (1,2,3,4)           ORDER BY 2 LIMIT 1;
SELECT (SELECT n1.id FROM names n1 ORDER BY n1.id LIMIT 1),firstname FROM names WHERE id IN (1,2,3,4,5)         ORDER BY 2 LIMIT 1;
SELECT (SELECT n1.id FROM names n1 ORDER BY n1.id LIMIT 1),firstname FROM names WHERE id IN (1,2,3,4,5,0)       ORDER BY 2 LIMIT 1;
SELECT (SELECT n1.id FROM names n1 ORDER BY n1.id LIMIT 1),firstname FROM names WHERE id IN (1,2,3,4,5,0,1,2,3) ORDER BY 2 LIMIT 1;

