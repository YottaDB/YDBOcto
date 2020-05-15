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

-- TNK01 : OCTO311 : Miscellaneous tests of NULL keyword using the `names` schema

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

-- Arithmetic Operators
SELECT 10 - NULL FROM names;
SELECT 10 * NULL FROM names;
SELECT 10 / NULL FROM names;
SELECT 10 % NULL FROM names;
SELECT 1*((col2::varchar)::integer) FROM ((SELECT NULL as col1, NULL AS col2)) u1;
-- Postgres issues an operator doesn't exist for integer * text operation error (when cast is not used) for the above query.
-- But, though similar values are tested below, no errors are reported by postgres as type is inferred here.
-- Octo works in both cases. These queries verify its behavior in both case.
SELECT id + 1 FROM ((SELECT NULL AS col1, 1 AS id) UNION (SELECT '', 3) UNION (SELECT 'Cool',4) UNION (SELECT NULL AS col1, NULL AS id)) u1 order by id;
SELECT id - 1 FROM ((SELECT NULL AS col1, 1 AS id) UNION (SELECT '', 3) UNION (SELECT 'Cool',4) UNION (SELECT NULL AS col1, NULL AS id)) u1 order by id;
SELECT id * 1 FROM ((SELECT NULL AS col1, 1 AS id) UNION (SELECT '', 3) UNION (SELECT 'Cool',4) UNION (SELECT NULL AS col1, NULL AS id)) u1 order by id;
SELECT id / 1 FROM ((SELECT NULL AS col1, 1 AS id) UNION (SELECT '', 3) UNION (SELECT 'Cool',4) UNION (SELECT NULL AS col1, NULL AS id)) u1 order by id;
SELECT 10 / id FROM ((SELECT NULL AS col1, 1 AS id) UNION (SELECT '', 2) UNION (SELECT 'Cool',5) UNION (SELECT NULL AS col1, NULL AS id)) u1 order by id;
SELECT 10 * id FROM ((SELECT NULL AS col1, 1 AS id) UNION (SELECT '', 3) UNION (SELECT 'Cool',4) UNION (SELECT NULL AS col1, NULL AS id)) u1 order by id;
SELECT 10 - id FROM ((SELECT NULL AS col1, 1 AS id) UNION (SELECT '', 3) UNION (SELECT 'Cool',4) UNION (SELECT NULL AS col1, NULL AS id)) u1 order by id;
SELECT 10 + id FROM ((SELECT NULL AS col1, 1 AS id) UNION (SELECT '', 3) UNION (SELECT 'Cool',4) UNION (SELECT NULL AS col1, NULL AS id)) u1 order by id;
-- -- Original query had u1.col1/u1.col2 in the where clause. But, to avoid postgres type checking CAST operation is performed.
SELECT * FROM ((SELECT NULL AS col1, NULL AS col2)) u1 WHERE ((u1.col1::varchar)::integer)/((u1.col2::varchar)::integer) = 5;
SELECT * FROM ((SELECT NULL AS col1, NULL AS col2)) u1 WHERE ((u1.col1::varchar)::integer)*((u1.col2::varchar)::integer) = 20;
SELECT ((col1::varchar)::integer)*((col2::varchar)::integer) FROM ((SELECT NULL AS col1, NULL AS col2)) u1;

-- Comparison Operator =
-- -- The result of comparing anything with a NULL value is NULL. It is expected select discards all rows when where clause evaluates to NULL.
SELECT * FROM names WHERE id = NULL;
SELECT * FROM ((SELECT NULL AS col1, 1 AS id) UNION (SELECT NULL AS col1, NULL AS id)) u1 WHERE id = NULL;
SELECT * FROM ((SELECT NULL AS col1, 1 AS id) UNION (SELECT NULL AS col1, NULL AS id)) u1 WHERE id != NULL;
SELECT * FROM (SELECT NULL AS col1, NULL AS col2) AS u1 WHERE u1.col1 = NULL;
SELECT COUNT(col2) FROM ((SELECT NULL AS col1, NULL AS col2) UNION (SELECT NULL AS col1, 1 AS col2)) u1 having COUNT(u1.col2) = NULL;
-- -- The result of comparing column values evaluates to true for only those rows which are not NULL
SELECT * FROM (SELECT NULL AS col1, NULL AS col2) AS u1 WHERE u1.col1 = u1.col2;
SELECT * FROM ((SELECT NULL AS col1, NULL AS col2) UNION (SELECT 1 AS col1, 1 AS col2)) AS u1 WHERE u1.col1 = u1.col2;

-- Logical AND
-- -- verifies three value logic of AND
-- -- TRUE AND UNKNOWN -> UNKNOWN
-- -- FALSE AND UNKNOWN -> FALSE
SELECT lastname = 'Cool' AND NULL FROM names;
-- -- UNKNOWN AND TRUE -> UNKNOWN
-- -- UNKNOWN AND FALSE -> FALSE
SELECT NULL AND lastname = 'Cool' FROM names;
-- -- UNKNOWN AND UNKNOWN -> UNKNOWN
SELECT NULL AND NULL FROM names;
-- -- Other misc tests
SELECT * FROM names WHERE lastname = 'Cool' AND NULL;
SELECT * FROM ((SELECT NULL AS col1, NULL AS col2) UNION (SELECT NULL AS col1, 1 AS col2)) AS u1 WHERE u1.col1 IS NULL AND u1.col2 = 1;
SELECT * FROM ((SELECT NULL AS col1, NULL AS col2) UNION (SELECT NULL AS col1, 1 AS col2)) AS u1 WHERE u1.col1 = NULL AND u1.col2 = 1;

-- Logical OR
-- -- verifies three value logic of OR
-- -- TRUE OR UNKNOWN -> TRUE
-- -- FALSE OR UNKNOWN -> UNKNOWN
SELECT lastname = 'Cool' OR NULL FROM names;
-- -- UNKNOWN OR TRUE -> TRUE
-- -- UNKNOWN OR FALSE -> UNKNOWN
SELECT NULL OR lastname = 'Cool' FROM names;
-- -e UNKNOWN OR UNKNOWN -> UNKNOWN
SELECT NULL OR NULL FROM names;
-- -- Other misc tests
SELECT * FROM names WHERE lastname='Cool' OR NULL;
SELECT * FROM ((SELECT NULL AS col1, NULL AS col2) UNION (SELECT NULL AS col1, 1 AS col2)) AS u1 WHERE u1.col1 = NULL OR u1.col2 = 1;
SELECT * FROM ((SELECT NULL AS col1, NULL AS col2) UNION (SELECT NULL AS col1, 1 AS col2)) AS u1 WHERE u1.col1 IS NULL OR u1.col2 = 1;

-- Logical NOT
-- verifies three value logic of NOT
SELECT NOT NULL;
-- -- Other misc tests with NOT
SELECT * FROM names WHERE NOT NULL;
SELECT NOT Null FROM names;

-- String concatenation operator
SELECT 'concatenation' || NULL || names.lastname FROM names;
SELECT 'concatenation' || col1 || 'test' FROM ((SELECT NULL AS col1, NULL AS col2) UNION (SELECT NULL AS col1, 1 AS col2)) u1;

-- Where clause
-- -- Rows for which the predicate evaluates to either FALSE or UNKNOWN are discarded by SELECT queries
SELECT names.id FROM names WHERE names.id = NULL;
SELECT COUNT(col2) FROM ((SELECT NULL AS col1, NULL AS col2) UNION (SELECT NULL AS col1, 1 AS col2)) u1 WHERE exists (SELECT u2.col2 FROM ((SELECT NULL AS col1, NULL AS col2) UNION (SELECT NULL AS col1, 1 AS col2)) u2 WHERE u2.col2 IS NULL);
SELECT * FROM ((SELECT NULL AS col1, NULL AS col2) UNION (SELECT NULL AS col1, 1 AS col2)) AS u1 WHERE u1.col2 IN (1,NULL);
SELECT * FROM ((SELECT NULL AS col1, NULL AS col2) UNION (SELECT NULL AS col1, 1 AS col2)) AS u1 WHERE u1.col2 BETWEEN 0 AND NULL;
-- -- CAST operation is performed to escape type checking in postgres
SELECT * FROM ((SELECT 0 AS col1, NULL AS col2, 3 AS col3 )) AS u1 WHERE (u1.col2 :: INTEGER) BETWEEN (u1.col1 :: INTEGER) AND (u1.col3 :: INTEGER);
-- Result should have 0 rows
SELECT * FROM (SELECT 1 AS col1, NULL AS col2) AS u1 WHERE u1.col1=(u1.col2 :: INTEGER);
SELECT * FROM (SELECT NULL AS col1, 1 AS col2) AS u1 WHERE (u1.col1 :: INTEGER)=u1.col2;

-- NULL specific comparison predicates
SELECT NULL is NULL;
SELECT NULL is NOT NULL;
SELECT * FROM (SELECT NULL AS col1) u1 WHERE u1.col1 is not NULL;
SELECT * FROM (SELECT NULL AS col1) u1 WHERE u1.col1 is NULL;
SELECT 'ok' FROM names WHERE (SELECT col1 FROM (SELECT 1 AS col1, NULL AS col2) AS n1) is null;
SELECT 'ok' FROM names WHERE (SELECT col1 FROM (SELECT NULL AS col1, NULL AS col2) AS n1) is not null;

-- JOIN
-- -- below queries verify JOIN using predicates having IS NULL and IS NOT NULL conditions
SELECT * FROM ((SELECT 0 AS customerid, 'Zoopacker' AS ordername) UNION (SELECT NULL AS customerid, 'Treble' AS ordername)) AS orders INNER JOIN ((SELECT 0 AS customerid, 'Louis' AS customername) UNION (SELECT NULL AS customerid, 'tom' AS customername)) AS customer ON (orders.customerid IS NULL AND customer.customerid IS NULL);
SELECT * FROM ((SELECT 0 AS customerid, 'Zoopacker' AS ordername) UNION (SELECT NULL AS customerid, 'Treble' AS ordername)) AS orders INNER JOIN ((SELECT 0 AS customerid, 'Louis' AS customername) UNION (SELECT NULL AS customerid, 'tom' AS customername)) AS customer ON (orders.customerid IS NOT NULL AND customer.customerid IS NOT NULL);
SELECT * FROM ((SELECT 0 AS customerid, 'Zoopacker' AS ordername) UNION (SELECT NULL AS customerid, 'Treble' AS ordername)) AS orders LEFT JOIN ((SELECT 0 AS customerid, 'Louis' AS customername) UNION (SELECT NULL AS customerid, 'tom' AS customername)) AS customer ON (orders.customerid IS NULL AND customer.customerid IS NULL);
SELECT * FROM ((SELECT 0 AS customerid, 'Zoopacker' AS ordername) UNION (SELECT NULL AS customerid, 'Treble' AS ordername)) AS orders RIGHT JOIN ((SELECT 0 AS customerid, 'Louis' AS customername) UNION (SELECT NULL AS customerid, 'tom' AS customername)) AS customer ON (orders.customerid IS NULL AND customer.customerid IS NULL);
SELECT * FROM ((SELECT 0 AS customerid, 'Zoopacker' AS ordername) UNION (SELECT NULL AS customerid, 'Treble' AS ordername)) AS orders LEFT JOIN ((SELECT 0 AS customerid, 'Louis' AS customername) UNION (SELECT NULL AS customerid, 'tom' AS customername)) AS customer ON (orders.customerid IS NOT NULL AND customer.customerid IS NOT NULL);
SELECT * FROM ((SELECT 0 AS customerid, 'Zoopacker' AS ordername) UNION (SELECT NULL AS customerid, 'Treble' AS ordername)) AS orders RIGHT JOIN ((SELECT 0 AS customerid, 'Louis' AS customername) UNION (SELECT NULL AS customerid, 'tom' AS customername)) AS customer ON (orders.customerid IS NOT NULL AND customer.customerid IS NOT NULL);

-- below queries verify the JOINs in the presence of NULL valued data
SELECT * FROM ((SELECT 0 AS customerid, 'Zoopacker' AS ordername) UNION (SELECT NULL AS customerid, 'Treble' AS ordername)) AS orders RIGHT OUTER JOIN ((SELECT 0 AS customerid, 'Louis' AS customername) UNION (SELECT NULL AS customerid, 'tom' AS customername)) AS customer ON orders.customerid=customer.customerid;
SELECT * FROM ((SELECT 0 AS customerid, 'Zoopacker' AS ordername) UNION (SELECT NULL AS customerid, 'Treble' AS ordername)) AS orders LEFT OUTER JOIN ((SELECT 0 AS customerid, 'Louis' AS customername) UNION (SELECT NULL AS customerid, 'tom' AS customername)) AS customer ON orders.customerid=customer.customerid;
SELECT * FROM ((SELECT 0 AS customerid, 'Zoopacker' AS ordername) UNION (SELECT NULL AS customerid, 'Treble' AS ordername)) AS orders FULL OUTER JOIN ((SELECT 0 AS customerid, 'Louis' AS customername) UNION (SELECT NULL AS customerid, 'tom' AS customername)) AS customer ON orders.customerid=customer.customerid;
SELECT * FROM ((SELECT 0 AS customerid, 'Zoopacker' AS ordername) UNION (SELECT NULL AS customerid, 'Treble' AS ordername)) AS orders INNER JOIN ((SELECT 0 AS customerid, 'Louis' AS customername) UNION (SELECT NULL AS customerid, 'tom' AS customername)) AS customer ON orders.customerid=customer.customerid;

-- CASE
SELECT CASE id WHEN NULL THEN 'is null' WHEN 0 THEN 'is zero' WHEN 1 THEN 'is one' end FROM ((SELECT 0 AS id) UNION (SELECT 1 AS id) UNION (SELECT NULL AS id)) n1 order by id;
SELECT CASE WHEN id IS NULL THEN 'is null' WHEN id=0 THEN 'is zero' WHEN id=1 THEN 'is one' end FROM ((SELECT 0 AS id) UNION (SELECT 1 AS id) UNION (SELECT NULL AS id)) n1 order by id;

-- ANY
select * from names n1 where n1.id > ANY (select 1 union select NULL union select 2);
select * from names n1 where NOT (n1.id > ANY (select 1 union select NULL union select 2));

-- ALL
select * from names n1 where n1.id > ALL (select 1 union select NULL::integer union select 2);
select * from names n1 where NOT (n1.id > ALL (select 1 union select NULL::integer union select 2));

-- IN with scalar list
select * from names n1 where (n1.id IN (1,NULL,2));
select * from names n1 where NOT (n1.id IN (1,NULL,2));
select * from names n1 where (NULL IN (1));
select * from names n1 where (NULL IN (1,NULL));
select * from names n1 where NOT (NULL IN (1));
select * from names n1 where NOT (NULL IN (1,NULL));

-- IN with subquery
select * from names n1 where (n1.id IN (select 1 union select NULL::integer union select 2));
select * from names n1 where NOT (n1.id IN (select 1 union select NULL::integer union select 2));
select * from names n1 where (NULL IN (select 1));
select * from names n1 where (NULL IN (select 1 union select NULL::integer));
select * from names n1 where NOT (NULL IN (select 1));
select * from names n1 where NOT (NULL IN (select 1 union select NULL::integer));

-- NOT IN with scalar list
select * from names n1 where (n1.id NOT IN (1,NULL,2));
select * from names n1 where NOT (n1.id NOT IN (1,NULL,2));
select * from names n1 where (NULL NOT IN (1));
select * from names n1 where (NULL NOT IN (1,NULL));
select * from names n1 where NOT (NULL NOT IN (1));
select * from names n1 where NOT (NULL NOT IN (1,NULL));

-- NOT IN with subquery
select * from names n1 where (n1.id NOT IN (select 1 union select NULL::integer union select 2));
select * from names n1 where NOT (n1.id NOT IN (select 1 union select NULL::integer union select 2));
select * from names n1 where (NULL NOT IN (select 1));
select * from names n1 where (NULL NOT IN (select 1 union select NULL::integer));
select * from names n1 where NOT (NULL NOT IN (select 1));
select * from names n1 where NOT (NULL NOT IN (select 1 union select NULL::integer));
-- NOT IN involving NULL on left side of NOT IN and no rows returned from right side of NOT IN
select * from names where NULL NOT IN (select id from names where id = 7);

-- Test edge cases in GetScalar^%ydboctoplanhelpers (mostly related to NULL handling)
select * from names where lastname = (select NULL);
select * from names where (select NULL) is NULL;
select * from names where NULL = (select NULL);
select * from names where NULL < (select NULL);
select * from names where lastname = (select ''::text);

-- Test column name is inherited fine from sub-query in presence of type casts
-- Note: This is unrelated to NULL handling but is placed here because this issue was noticed while testing for NULL.
select id from (select id::boolean from names) n1;

-- Test NULL and typecast to boolean
select col1 from ((select NULL::boolean as col1) union (select true as col1)) subquery1 where col1 is NULL;

