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

-- TVC01 : OCTO502 : Implement VALUES clause (aka table-value-constructor)

-- Test simplest query using VALUES clause (1 row, 1 column) and INTEGER type
VALUES (1);

-- Test VALUES clause using more than one column (1 row, 3 columns) and INTEGER type
VALUES (1,2,3);

-- Test VALUES clause using more than one row and column (2 row, 3 columns) and INTEGER type
VALUES (1,2,3), (4,5,6);

-- Test VALUES clause in subquery
SELECT * FROM (VALUES (1)) as MyTable;

-- Test VALUES clause using VARCHAR data type
SELECT * FROM (VALUES ('abcd')) AS tvc;

-- Test VALUES clause using BOOLEAN data type
SELECT * FROM (VALUES (true, false)) AS tvc;

-- Test non-integer numeric literals
SELECT * FROM (VALUES (1), (1.5)) AS tvc;

-- Test fancier VALUES clause in subquery
SELECT column3,column2,column1 FROM (VALUES (4,5,6), (1,2,3)) AS tvc;

-- Test queries with VALUES in arithmetic operations
SELECT 5 + (VALUES (3));

-- Test VALUES clause as input in the JOIN table list
SELECT * FROM (VALUES (1)) n1 INNER JOIN (VALUES (2)) n2 ON n1.column1 < n2.column1;

-- Slightly fancier queries
SELECT column1 FROM (VALUES (1)) as MyTable;
SELECT * FROM (VALUES (1), (3), (5), (7), (9) ) AS MyTable;
SELECT * FROM (VALUES (1, 2), (3, 4), (5, 6), (7, 8), (9, 10) ) AS MyTable;
SELECT * FROM (VALUES ((SELECT 1), 2)) as abcd;
SELECT * FROM (VALUES ((SELECT 1), (SELECT 2), 3)) as abcd;
SELECT * FROM (VALUES (1, (SELECT 2))) as abcd;
VALUES((SELECT id FROM names WHERE id > 4));
VALUES ((VALUES (1)));
VALUES (1 + (VALUES (1)));

-- Test that column names are by default "column1", "column2" etc.
SELECT column2,column1 FROM (VALUES (1, 2), (3, 4), (5, 6), (7, 8), (9, 10) ) AS MyTable;
SELECT column1,column3,column2,column2 FROM (VALUES (4,5,(SELECT id FROM names WHERE id = 1))) n1;

-- Test of NULL
VALUES (NULL);
VALUES((SELECT id FROM names WHERE id > 5));
SELECT id FROM names WHERE id = (SELECT * FROM (VALUES (NULL),(1)) n1 WHERE column1 = 1);
SELECT id FROM names WHERE id = (SELECT * FROM (VALUES (NULL),(1)) n1 WHERE column1 IS NULL);
SELECT id FROM names WHERE id = (SELECT * FROM (VALUES (NULL),(1)) n1 WHERE column1 IS NOT NULL);
SELECT id from names n1 WHERE (SELECT n1.id FROM names n2 WHERE n2.id > 5) IS NULL;
SELECT id from names n1 WHERE (SELECT n1.id FROM names n2 WHERE n2.id > 5) IS NOT NULL;

-- Test of SET operations (UNION, INTERSECT etc.)
SELECT * FROM names where id < 4 UNION ALL (VALUES (0, 'Zero', 'Cool'), (1, 'Acid', 'Burn'), (2, 'Cereal', 'Killer'), (3, 'Lord', 'Nikon'), (4, 'Joey', ''), (5, 'Zero', 'Cool'));
SELECT * FROM names where id < 4 INTERSECT ALL (VALUES (0, 'Zero', 'Cool'), (1, 'Acid', 'Burn'), (2, 'Cereal', 'Killer'), (3, 'Lord', 'Nikon'), (4, 'Joey', ''), (5, 'Zero', 'Cool'));
SELECT * FROM names where id < 4 EXCEPT ALL (VALUES (0, 'Zero', 'Cool'), (1, 'Acid', 'Burn'), (2, 'Cereal', 'Killer'), (3, 'Lord', 'Nikon'), (4, 'Joey', ''), (5, 'Zero', 'Cool'));
SELECT * FROM names where id < 4 UNION (VALUES (0, 'Zero', 'Cool'), (1, 'Acid', 'Burn'), (2, 'Cereal', 'Killer'), (3, 'Lord', 'Nikon'), (4, 'Joey', ''), (5, 'Zero', 'Cool'));
SELECT * FROM names where id < 4 INTERSECT (VALUES (0, 'Zero', 'Cool'), (1, 'Acid', 'Burn'), (2, 'Cereal', 'Killer'), (3, 'Lord', 'Nikon'), (4, 'Joey', ''), (5, 'Zero', 'Cool'));
SELECT * FROM names where id < 4 EXCEPT (VALUES (0, 'Zero', 'Cool'), (1, 'Acid', 'Burn'), (2, 'Cereal', 'Killer'), (3, 'Lord', 'Nikon'), (4, 'Joey', ''), (5, 'Zero', 'Cool'));
SELECT * FROM names where id < 4 UNION ALL VALUES (0, 'Zero', 'Cool'), (1, 'Acid', 'Burn'), (2, 'Cereal', 'Killer'), (3, 'Lord', 'Nikon'), (4, 'Joey', ''), (5, 'Zero', 'Cool');
SELECT * FROM names where id < 4 INTERSECT ALL VALUES (0, 'Zero', 'Cool'), (1, 'Acid', 'Burn'), (2, 'Cereal', 'Killer'), (3, 'Lord', 'Nikon'), (4, 'Joey', ''), (5, 'Zero', 'Cool');
SELECT * FROM names where id < 4 EXCEPT ALL VALUES (0, 'Zero', 'Cool'), (1, 'Acid', 'Burn'), (2, 'Cereal', 'Killer'), (3, 'Lord', 'Nikon'), (4, 'Joey', ''), (5, 'Zero', 'Cool');
SELECT * FROM names where id < 4 UNION VALUES (0, 'Zero', 'Cool'), (1, 'Acid', 'Burn'), (2, 'Cereal', 'Killer'), (3, 'Lord', 'Nikon'), (4, 'Joey', ''), (5, 'Zero', 'Cool');
SELECT * FROM names where id < 4 INTERSECT VALUES (0, 'Zero', 'Cool'), (1, 'Acid', 'Burn'), (2, 'Cereal', 'Killer'), (3, 'Lord', 'Nikon'), (4, 'Joey', ''), (5, 'Zero', 'Cool');
SELECT * FROM names where id < 4 EXCEPT VALUES (0, 'Zero', 'Cool'), (1, 'Acid', 'Burn'), (2, 'Cereal', 'Killer'), (3, 'Lord', 'Nikon'), (4, 'Joey', ''), (5, 'Zero', 'Cool');
VALUES (0, 'Zero', 'Cool'), (1, 'Acid', 'Burn'), (2, 'Cereal', 'Killer'), (3, 'Lord', 'Nikon'), (4, 'Joey', ''), (5, 'Zero', 'Cool') UNION ALL SELECT * FROM names where id < 4;
VALUES (0, 'Zero', 'Cool'), (1, 'Acid', 'Burn'), (2, 'Cereal', 'Killer'), (3, 'Lord', 'Nikon'), (4, 'Joey', ''), (5, 'Zero', 'Cool') INTERSECT ALL SELECT * FROM names where id < 4;
VALUES (0, 'Zero', 'Cool'), (1, 'Acid', 'Burn'), (2, 'Cereal', 'Killer'), (3, 'Lord', 'Nikon'), (4, 'Joey', ''), (5, 'Zero', 'Cool') EXCEPT ALL SELECT * FROM names where id < 4;
VALUES (0, 'Zero', 'Cool'), (1, 'Acid', 'Burn'), (2, 'Cereal', 'Killer'), (3, 'Lord', 'Nikon'), (4, 'Joey', ''), (5, 'Zero', 'Cool') UNION SELECT * FROM names where id < 4;
VALUES (0, 'Zero', 'Cool'), (1, 'Acid', 'Burn'), (2, 'Cereal', 'Killer'), (3, 'Lord', 'Nikon'), (4, 'Joey', ''), (5, 'Zero', 'Cool') INTERSECT SELECT * FROM names where id < 4;
VALUES (0, 'Zero', 'Cool'), (1, 'Acid', 'Burn'), (2, 'Cereal', 'Killer'), (3, 'Lord', 'Nikon'), (4, 'Joey', ''), (5, 'Zero', 'Cool') EXCEPT SELECT * FROM names where id < 4;

-- Test of IN
SELECT * FROM NAMES WHERE id IN (values (2));
SELECT * FROM NAMES WHERE id IN (values (3), (4), (6), (7));

-- Test of EXISTS
SELECT * FROM names WHERE EXISTS (VALUES (2));
SELECT * FROM names WHERE NOT EXISTS (VALUES (2));
SELECT * FROM names WHERE EXISTS (VALUES ((SELECT id FROM names WHERE id > 7)));
SELECT * FROM names WHERE NOT EXISTS (VALUES ((SELECT id FROM names WHERE id > 7)));

-- Test nested VALUES clause
VALUES ((SELECT column1 FROM (VALUES (2)) n2));
VALUES (1), ((SELECT COLUMN1 FROM (VALUES ((VALUES ((SELECT column1 FROM (VALUES (3)) n3))))) n2));

