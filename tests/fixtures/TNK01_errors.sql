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

-- All queries in this query file are invalid queries and generate an error.

-- TNK01 : OCTO311 : Miscellaneous tests of NULL keyword

-- Sub-query that returns a NULL ( > 1 row, == 1 column)
SELECT * FROM names a WHERE a.firstName = (SELECT NULL from names limit 2);
SELECT * FROM names a WHERE a.firstName != (SELECT NULL from names limit 2);

SELECT * FROM names WHERE 2;
SELECT * FROM names WHERE 2 OR 1=1;
SELECT * FROM names WHERE 2 AND 1=1;
SELECT * FROM names WHERE 2 AND NOT 1=1;
SELECT * FROM names WHERE NOT 2;
SELECT * FROM names WHERE NOT 2 OR 1=1;
SELECT * FROM names WHERE NOT 2 AND 1=1;
SELECT * FROM names WHERE NOT 2 AND NOT 1=1;

SELECT * FROM names WHERE 'abcd';
SELECT * FROM names WHERE 'abcd' OR 1=1;
SELECT * FROM names WHERE 'abcd' AND 1=1;
SELECT * FROM names WHERE 'abcd' AND NOT 1=1;
SELECT * FROM names WHERE NOT 'abcd';
SELECT * FROM names WHERE NOT 'abcd' OR 1=1;
SELECT * FROM names WHERE NOT 'abcd' AND 1=1;
SELECT * FROM names WHERE NOT 'abcd' AND NOT 1=1;

SELECT * FROM names WHERE 'abcd'::boolean;
SELECT * FROM names WHERE 'abcd'::boolean OR 1=1;
SELECT * FROM names WHERE 'abcd'::boolean AND 1=1;
SELECT * FROM names WHERE 'abcd'::boolean AND NOT 1=1;
SELECT * FROM names WHERE NOT 'abcd'::boolean;
SELECT * FROM names WHERE NOT 'abcd'::boolean OR 1=1;
SELECT * FROM names WHERE NOT 'abcd'::boolean AND 1=1;
SELECT * FROM names WHERE NOT 'abcd'::boolean AND NOT 1=1;

-- Below query works fine in Octo but errors out in Postgres (in at least one version that is being used in the pipeline)
-- hence is included in TNK01_errors.sql instead of in TNK01_noerrors.sql
SELECT 10 + (select NULL) FROM names;
SELECT 10 + (select NULL)::INTEGER FROM names;
SELECT * FROM names a WHERE a.firstName = (SELECT NULL);
SELECT * FROM names a WHERE a.firstName != (SELECT NULL);
SELECT * FROM (SELECT NULL AS col1, 1 AS col2) AS u1 WHERE u1.col1::INTEGER = u1.col2::INTEGER;
SELECT 1*col2::INTEGER from ((SELECT NULL AS col1, NULL AS col2)) u1;
SELECT * from ((SELECT 0 AS col1, NULL AS col2, 3 AS col3 )) AS u1 WHERE u1.col2::INTEGER BETWEEN u1.col1 AND u1.col3;
SELECT * FROM ((SELECT NULL AS col1, NULL AS col2)) u1 WHERE u1.col1::INTEGER/u1.col2::INTEGER = 5;

