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

-- TERR030 : OCTO502 : Error scenarios in VALUES

-- Test of ERR_VALUES_LENGTH

SELECT * FROM (VALUES (1), (2,3)) AS MyTable;
SELECT * FROM (VALUES (1), (2), (3), (2,3)) AS MyTable;

-- Test of ERR_TYPE_MISMATCH
SELECT * FROM (VALUES (1), ('abcd')) AS abcd;
SELECT * FROM (VALUES (1), (1.5), (true)) AS abcd;
SELECT id FROM names WHERE id = (SELECT * FROM (VALUES (NULL),('abcd')) n1 WHERE column1 = 1);
SELECT id FROM names WHERE id = (SELECT * FROM (VALUES (NULL),('abcd')) n1 WHERE column1 IS NOT NULL);

-- Test of ERR_UNKNOWN_COLUMN_NAME
SELECT a, b FROM (VALUES (1, 2), (3, 4), (5, 6), (7, 8), (9, 10) ) AS MyTable;
SELECT col1 FROM (VALUES (1)) as MyTable;

-- Test of Syntax error
VALUES (VALUES (1));
SELECT * FROM (VALUES (1, SELECT 1)) AS abcd;

-- Test of ERR_SUBQUERY_MULTIPLE_ROWS
VALUES((SELECT id FROM names));
SELECT id FROM names WHERE id = (SELECT * FROM (VALUES (NULL),(1)) n1);

-- Test of ERR_SETOPER_TYPE_MISMATCH
SELECT firstname,lastname,id FROM names UNION ALL (VALUES (0, 'Zero', 'Cool'), (1, 'Acid', 'Burn'), (2, 'Cereal', 'Killer'), (3, 'Lord', 'Nikon'), (4, 'Joey', ''), (5, 'Zero', 'Cool'));

-- Test of ERR_SUBQUERY_ONE_COLUMN
SELECT * FROM NAMES WHERE id IN (values (3,4,6,7));

