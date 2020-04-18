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

-- TNK01 : OCTO311 : Miscellaneous tests of NULL keyword

-- Test of IS NULL vs = NULL
select * from names n1 where n1.id is NULL;
select * from names n1 where n1.id is NOT NULL;
select * from names n1 where n1.id = NULL;
select * from names n1 where n1.id != NULL;

-- Test of a mix of NULL and empty string (inherited from a sub-query) with ORDER BY, GROUP BY, DISTINCT etc.
select id,col1 from ((select NULL as col1, 1 as id) union (select 'First', 2) union (select '', 3) union (select 'Cool',4) union (select 'Cool',5)) u1;
select id,col1 from ((select NULL as col1, 1 as id) union (select 'First', 2) union (select '', 3) union (select 'Cool',4) union (select 'Cool',5)) u1 order by col1, id;
select id,col1 from ((select NULL as col1, 1 as id) union (select 'First', 2) union (select '', 3) union (select 'Cool',4) union (select 'Cool',5)) u1 order by col1 desc, id;
select id,col1 from ((select NULL as col1, 1 as id) union (select 'First', 2) union (select '', 3) union (select 'Cool',4) union (select 'Cool',5)) u1 order by id, col1;
select id,col1 from ((select NULL as col1, 1 as id) union (select 'First', 2) union (select '', 3) union (select 'Cool',4) union (select 'Cool',5)) u1 order by id, col1 desc;
select AVG(id),col1 from ((select NULL as col1, 1 as id) union (select 'First', 2) union (select '', 3) union (select 'Cool',4) union (select 'Cool',5)) u1 GROUP BY col1 order by col1;
select MIN(id),col1 from ((select NULL as col1, 1 as id) union (select 'First', 2) union (select '', 3) union (select 'Cool',4) union (select 'Cool',5)) u1 GROUP BY col1 order by col1;
select MAX(id),col1 from ((select NULL as col1, 1 as id) union (select 'First', 2) union (select '', 3) union (select 'Cool',4) union (select 'Cool',5)) u1 GROUP BY col1 order by col1 desc;
select SUM(id),col1 from ((select NULL as col1, 1 as id) union (select 'First', 2) union (select '', 3) union (select 'Cool',4) union (select 'Cool',5)) u1 GROUP BY col1 order by col1;
select COUNT(id),col1 from ((select NULL as col1, 1 as id) union (select 'First', 2) union (select '', 3) union (select 'Cool',4) union (select 'Cool',5)) u1 GROUP BY col1 order by col1;
select COUNT(*),col1 from ((select NULL as col1, 1 as id) union (select 'First', 2) union (select '', 3) union (select 'Cool',4) union (select 'Cool',5)) u1 GROUP BY col1 order by col1;
select * from ((select NULL as col1, 1 as id) union (select 'First', 2) union (select '', 3) union (select 'Cool',4) union (select 'Cool',5)) u1;
select * from ((select NULL as col1, 1 as id) union (select 'First', 2) union (select '', 3) union (select 'Cool',4) union (select 'Cool',5)) u1 order by col1, id;
select * from ((select NULL as col1, 1 as id) union (select 'First', 2) union (select '', 3) union (select 'Cool',4) union (select 'Cool',5)) u1 order by col1 desc, id;
select * from ((select NULL as col1, 1 as id) union (select 'First', 2) union (select '', 3) union (select 'Cool',4) union (select 'Cool',5)) u1 order by id, col1;
select * from ((select NULL as col1, 1 as id) union (select 'First', 2) union (select '', 3) union (select 'Cool',4) union (select 'Cool',5)) u1 order by id, col1 desc;
select distinct col1 from ((select NULL as col1, 1 as id) union (select 'First', 2) union (select '', 3) union (select 'Cool',4) union (select 'Cool',5)) u1 order by col1 desc;

-- Tests that were added for #192 but failed because they were waiting for #311 to be fixed.
-- Misc queries that did not work until OCTO311 was fixed
-- Below has a subquery that returns 0 rows. The return value when used in an expression should be NULL. This is a test for that.
SELECT * FROM names a WHERE a.firstName != (SELECT b.firstName FROM names b WHERE a.id = (b.id + 2));
SELECT * FROM names a WHERE a.firstName != (SELECT b.firstName FROM names b WHERE -a.id = b.id);
-- Below has a subquery that returns NULL.
SELECT * FROM names a WHERE a.firstName = (SELECT NULL::text);
SELECT * FROM names a WHERE a.firstName != (SELECT NULL::text);

-- Below queries are from issue description of YDBOcto#311 on gitlab
SELECT * FROM names n1 WHERE n1.id IS NULL;
SELECT * FROM names n1 WHERE n1.id IS NOT NULL;
SELECT * FROM names n1 WHERE n1.id = NULL;
SELECT * FROM names n1 WHERE n1.id != NULL;
SELECT * FROM names WHERE firstname > NULL;
SELECT * FROM names WHERE firstname >= NULL;
SELECT * FROM names WHERE firstname < NULL;
SELECT * FROM names WHERE firstname <= NULL;
SELECT * FROM names WHERE firstname != NULL;
SELECT * FROM names WHERE firstname BETWEEN NULL AND 'MIDPOINT';
SELECT * FROM names WHERE firstname BETWEEN 'MIDPOINT' AND NULL;
SELECT * FROM names WHERE id > NULL;
SELECT * FROM names WHERE id >= NULL;
SELECT * FROM names WHERE id < NULL;
SELECT * FROM names WHERE id <= NULL;
SELECT * FROM names WHERE id != NULL;
SELECT * FROM names WHERE id BETWEEN NULL AND 3;
SELECT * FROM names WHERE id BETWEEN 3 AND NULL;

-- Below queries test that NULL/TRUE/FALSE can be used as a boolean expression too
SELECT * FROM names WHERE NULL;
SELECT * FROM names WHERE NULL::boolean;
SELECT * FROM names WHERE NULL OR 1=1;
SELECT * FROM names WHERE NULL AND 1=1;
SELECT * FROM names WHERE NULL AND NOT 1=1;
SELECT * FROM names WHERE NOT NULL;
SELECT * FROM names WHERE NOT NULL OR 1=1;
SELECT * FROM names WHERE NOT NULL AND 1=1;
SELECT * FROM names WHERE NOT NULL AND NOT 1=1;
SELECT * FROM names WHERE NOT NULL::boolean;

SELECT * FROM names WHERE TRUE;
SELECT * FROM names WHERE TRUE::boolean;
SELECT * FROM names WHERE TRUE OR 1=1;
SELECT * FROM names WHERE TRUE AND 1=1;
SELECT * FROM names WHERE TRUE AND NOT 1=1;
SELECT * FROM names WHERE NOT TRUE;
SELECT * FROM names WHERE NOT TRUE OR 1=1;
SELECT * FROM names WHERE NOT TRUE AND 1=1;
SELECT * FROM names WHERE NOT TRUE AND NOT 1=1;
SELECT * FROM names WHERE NOT TRUE::boolean;

SELECT * FROM names WHERE FALSE;
SELECT * FROM names WHERE FALSE::boolean;
SELECT * FROM names WHERE FALSE OR 1=1;
SELECT * FROM names WHERE FALSE AND 1=1;
SELECT * FROM names WHERE FALSE AND NOT 1=1;
SELECT * FROM names WHERE NOT FALSE;
SELECT * FROM names WHERE NOT FALSE OR 1=1;
SELECT * FROM names WHERE NOT FALSE AND 1=1;
SELECT * FROM names WHERE NOT FALSE AND NOT 1=1;
SELECT * FROM names WHERE NOT FALSE::boolean;

SELECT * FROM names WHERE 2::boolean;
SELECT * FROM names WHERE 2::boolean OR 1=1;
SELECT * FROM names WHERE 2::boolean AND 1=1;
SELECT * FROM names WHERE 2::boolean AND NOT 1=1;
SELECT * FROM names WHERE NOT 2::boolean;
SELECT * FROM names WHERE NOT 2::boolean OR 1=1;
SELECT * FROM names WHERE NOT 2::boolean AND 1=1;
SELECT * FROM names WHERE NOT 2::boolean AND NOT 1=1;

-- Below queries test various arithmetic and other operations on NULL works as expected
SELECT 10 + NULL FROM names;
SELECT id + NULL FROM names;
SELECT 'concatenation' || NULL || 'operator' AS result_string;
SELECT 'concatenation' || NULL || names.lastname FROM names;
SELECT NULL || NULL;
SELECT * FROM ((SELECT 'Cool' AS col1, NULL AS id) UNION (SELECT 'First' AS col1, 1 AS id)) AS u1 INNER JOIN ((SELECT 'Cool' AS col1, NULL AS id) UNION (SELECT 'Last' AS col1, 2 AS id)) AS u2 ON u1.col1 = u2.col1;
SELECT * FROM ((SELECT 'Cool' AS col1, NULL AS id) UNION (SELECT 'First' AS col1, 1 AS id)) AS u1 INNER JOIN ((SELECT 'Cool' AS col1, NULL AS id) UNION (SELECT 'Last' AS col1, 2 AS id)) AS u2 ON u1.id = u2.id;
SELECT * FROM (SELECT NULL::integer AS col1, 1 AS col2) AS u1 WHERE u1.col1 = u1.col2;
SELECT id = 1 FROM ((SELECT NULL AS col1, 1 AS id) UNION (SELECT 'First', 2) UNION (SELECT NULL AS col1, NULL AS id)) u1 ORDER BY id;
SELECT * from ((SELECT NULL AS col1, NULL AS col2) UNION (SELECT 10 AS col1, 2 AS col2)) u1 WHERE u1.col1/u1.col2 = 5;
SELECT id * 1 from ((SELECT NULL AS col1, 1 AS id) UNION (SELECT '', 3) UNION (SELECT 'Cool',4) UNION (SELECT NULL AS col1, NULL AS id)) u1 order by id;
SELECT 1*col2::INTEGER from ((SELECT 1 AS col1, 1 AS col2) UNION (SELECT NULL AS col1, NULL AS col2)) u1;
SELECT 1*col2 from ((SELECT NULL AS col1, NULL::integer AS col2)) u1;
SELECT * from ((SELECT NULL AS col1, NULL AS col2) UNION (SELECT NULL AS col1, 1 AS col2)) AS u1 WHERE u1.col2 IN (NULL+1);
SELECT * from ((SELECT NULL AS col1, NULL AS col2) UNION (SELECT NULL AS col1, 1 AS col2)) AS u1 WHERE u1.col2 BETWEEN 0+u1.col1::INTEGER AND 2;
SELECT * from ((SELECT NULL AS col1, NULL AS col2) UNION (SELECT NULL AS col1, 1 AS col2)) AS u1 WHERE u1.col2 BETWEEN 0+NULL AND 2;
SELECT COUNT(col2) from ((SELECT NULL AS col1, NULL AS col2) UNION (SELECT NULL AS col1, 1 AS col2)) u1 WHERE EXISTS (SELECT u2.col2 from ((SELECT NULL AS col1, NULL AS col2) UNION (SELECT NULL AS col1, 1 AS col2)) u2 WHERE u2.col2=1);
SELECT COUNT(col2) from ((SELECT NULL AS col1, NULL AS col2) UNION (SELECT NULL AS col1, 1 AS col2)) u1 WHERE EXISTS (SELECT u2.col2 from ((SELECT NULL AS col1, NULL AS col2) UNION (SELECT NULL AS col1, 1 AS col2)) u2 WHERE u2.col2 IS NULL);
SELECT COUNT(col2) from ((SELECT NULL AS col1, NULL AS col2) UNION (SELECT NULL AS col1, 1 AS col2)) u1 WHERE EXISTS (SELECT col2 from ((SELECT NULL AS col1, NULL AS col2) UNION (SELECT NULL AS col1, 1 AS col2)) u2 WHERE u2.col2=NULL);
SELECT NULL OR NOT NULL from names;
SELECT NULL AND NOT NULL from names;
SELECT NULL OR NULL from names;
SELECT NULL AND NULL from names;
SELECT * from ((SELECT 0 AS col1, 1 AS col2, 3 AS col3 )) AS u1 WHERE u1.col2 BETWEEN u1.col1 AND u1.col3;
SELECT * from ((SELECT 0 AS col1, NULL::integer AS col2, 3 AS col3 )) AS u1 WHERE u1.col2 BETWEEN u1.col1 AND u1.col3;
SELECT 'ok' from names WHERE (SELECT col1 from (SELECT 1 AS col1, NULL AS col2) AS n1) IS NOT NULL;
SELECT 'ok' from names WHERE (SELECT col1 from (SELECT NULL AS col1, NULL AS col2) AS n1) IS NULL;
SELECT * FROM ((SELECT NULL::integer AS col1, NULL::integer AS col2)) u1 WHERE u1.col1/u1.col2 is NULL;

