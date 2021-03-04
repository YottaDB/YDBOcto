#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TX04 : OCTO531 : Optimize IS NULL check in WHERE clause by using key fixing optimization

-- Test of columns on left side of IS NULL and IS NOT NULL
SELECT * FROM names WHERE lastname IS NULL;			-- expect key fixing
SELECT * FROM names WHERE lastname IS NOT NULL;			-- do not expect key fixing

-- Test of expressions on left side of IS NULL and IS NOT NULL
SELECT * FROM names WHERE (lastname || firstname) IS NULL;	-- do not expect key fixing
SELECT * FROM names WHERE (lastname || firstname) IS NOT NULL;	-- do not expect key fixing

-- Test of constants on left side of IS NULL and IS NOT NULL
SELECT * FROM names WHERE 'abcd' IS NULL;			-- do not expect key fixing
SELECT * FROM names WHERE 2 IS NOT NULL;			-- do not expect key fixing

-- Additional test of IS NULL with INNER and OUTER JOINs
-- INNER JOINs expect at least 1 key fixing. OUTER JOINs expect no key fixing.

SELECT * FROM names n1, names n2                   WHERE n1.lastname IS NULL;
SELECT * FROM names n1 INNER JOIN names n2 ON TRUE WHERE n1.lastname IS NULL;
SELECT * FROM names n1 LEFT JOIN  names n2 ON TRUE WHERE n1.lastname IS NULL;
SELECT * FROM names n1 RIGHT JOIN names n2 ON TRUE WHERE n1.lastname IS NULL;
SELECT * FROM names n1 FULL JOIN  names n2 ON TRUE WHERE n1.lastname IS NULL;

SELECT * FROM names n1, names n2                   WHERE n2.lastname IS NULL;
SELECT * FROM names n1 INNER JOIN names n2 ON TRUE WHERE n2.lastname IS NULL;
SELECT * FROM names n1 LEFT JOIN  names n2 ON TRUE WHERE n2.lastname IS NULL;
SELECT * FROM names n1 RIGHT JOIN names n2 ON TRUE WHERE n2.lastname IS NULL;
SELECT * FROM names n1 FULL JOIN  names n2 ON TRUE WHERE n2.lastname IS NULL;

SELECT * FROM names n1, names n2                   WHERE (n1.lastname IS NULL) AND (n2.lastname IS NULL);
SELECT * FROM names n1 INNER JOIN names n2 ON TRUE WHERE (n1.lastname IS NULL) AND (n2.lastname IS NULL);
SELECT * FROM names n1 LEFT JOIN  names n2 ON TRUE WHERE (n1.lastname IS NULL) AND (n2.lastname IS NULL);
SELECT * FROM names n1 RIGHT JOIN names n2 ON TRUE WHERE (n1.lastname IS NULL) AND (n2.lastname IS NULL);
SELECT * FROM names n1 FULL JOIN  names n2 ON TRUE WHERE (n1.lastname IS NULL) AND (n2.lastname IS NULL);

SELECT * FROM names n1, names n2                   WHERE (n1.lastname IS NULL) OR  (n2.lastname IS NULL);
SELECT * FROM names n1 INNER JOIN names n2 ON TRUE WHERE (n1.lastname IS NULL) OR  (n2.lastname IS NULL);
SELECT * FROM names n1 LEFT JOIN  names n2 ON TRUE WHERE (n1.lastname IS NULL) OR  (n2.lastname IS NULL);
SELECT * FROM names n1 RIGHT JOIN names n2 ON TRUE WHERE (n1.lastname IS NULL) OR  (n2.lastname IS NULL);
SELECT * FROM names n1 FULL JOIN  names n2 ON TRUE WHERE (n1.lastname IS NULL) OR  (n2.lastname IS NULL);

-- Test of IS NULL in ON clause with 2 JOINs
SELECT * FROM names n1 INNER JOIN names n2 ON n2.lastname IS NULL;
SELECT * FROM names n1 LEFT  JOIN names n2 ON n2.lastname IS NULL;
SELECT * FROM names n1 RIGHT JOIN names n2 ON n2.lastname IS NULL;
-- Below query is commented out because Postgres does not support FULL JOIN with IS NULL in ON clause.
-- SELECT * FROM names n1 FULL  JOIN names n2 ON n2.lastname IS NULL;

-- Test of IS NULL in ON clause with 3 JOINs
SELECT * FROM names n1 INNER JOIN names n2 ON n2.lastname IS NULL INNER JOIN names n3 ON n3.firstname = n2.firstname WHERE n1.firstname IS NULL;
SELECT * FROM names n1 LEFT  JOIN names n2 ON n2.lastname IS NULL INNER JOIN names n3 ON n3.firstname = n2.firstname WHERE n1.firstname IS NULL;
SELECT * FROM names n1 RIGHT JOIN names n2 ON n2.lastname IS NULL INNER JOIN names n3 ON n3.firstname = n2.firstname WHERE n1.firstname IS NULL;
-- Below query is commented out because Postgres does not support FULL JOIN with IS NULL in ON clause.
-- SELECT * FROM names n1 FULL  JOIN names n2 ON n2.lastname IS NULL INNER JOIN names n3 ON n3.firstname = n2.firstname WHERE n1.firstname IS NULL;

