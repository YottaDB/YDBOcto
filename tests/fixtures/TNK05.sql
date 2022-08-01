#################################################################
#                                                               #
# Copyright (c) 2021-2023 YottaDB LLC and/or its subsidiaries.       #
# All rights reserved.                                          #
#                                                               #
#       This source code contains the intellectual property     #
#       of its copyright holder(s), and is made available       #
#       under a license.  If you do not know the terms of       #
#       the license, please stop and do not read further.       #
#                                                               #
#################################################################

-- TNK05 : OCTO687 : Test of '' vs NULL

-- All queries below use '' and so currently behave differently in Octo when compared to Postgres.
-- Therefore they cannot be included in the .sql files identified below in the comments because
-- those tests use "run_query_in_octo_and_postgres_and_crosscheck_multiple_queries".
-- These can be moved to the individual .sql files in the comments below when YDBOcto#687 is fixed
-- so Octo handling of '' is same as Postgres.

-- Below queries are from tests/fixtures/TRTR04a.sql
select firstname from ((select * from names) union (select 8 as id,'ey' as firstname, '' as lastname))n1 where firstname SIMILAR TO (firstname || lastname);
select firstname from ((select * from names) union (select 8 as id,'ey' as firstname, '' as lastname))n1 where firstname LIKE (firstname || lastname);
select * from ((select * from names) union (select 8 as id,'ey' as firstname, '' as lastname))n1 where (firstname || lastname) SIMILAR TO firstname;
select * from ((select * from names) union (select 8 as id,'ey' as firstname, '' as lastname))n1 where (firstname || lastname) LIKE firstname;
select * from ((select * from names) union (select 8 as id,'ey' as firstname, '' as lastname))n1 where (firstname || lastname) ~ firstname;

-- Below queries are from tests/fixtures/TNK01_noerrors_names.sql
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
SELECT id * 1 from ((SELECT NULL AS col1, 1 AS id) UNION (SELECT '', 3) UNION (SELECT 'Cool',4) UNION (SELECT NULL AS col1, NULL AS id)) u1 order by id;
SELECT id + 1 FROM ((SELECT NULL AS col1, 1 AS id) UNION (SELECT '', 3) UNION (SELECT 'Cool',4) UNION (SELECT NULL AS col1, NULL AS id)) u1 order by id;
SELECT id - 1 FROM ((SELECT NULL AS col1, 1 AS id) UNION (SELECT '', 3) UNION (SELECT 'Cool',4) UNION (SELECT NULL AS col1, NULL AS id)) u1 order by id;
SELECT id * 1 FROM ((SELECT NULL AS col1, 1 AS id) UNION (SELECT '', 3) UNION (SELECT 'Cool',4) UNION (SELECT NULL AS col1, NULL AS id)) u1 order by id;
SELECT id / 1 FROM ((SELECT NULL AS col1, 1 AS id) UNION (SELECT '', 3) UNION (SELECT 'Cool',4) UNION (SELECT NULL AS col1, NULL AS id)) u1 order by id;
SELECT 10 / id FROM ((SELECT NULL AS col1, 1 AS id) UNION (SELECT '', 2) UNION (SELECT 'Cool',5) UNION (SELECT NULL AS col1, NULL AS id)) u1 order by id;
SELECT 10 * id FROM ((SELECT NULL AS col1, 1 AS id) UNION (SELECT '', 3) UNION (SELECT 'Cool',4) UNION (SELECT NULL AS col1, NULL AS id)) u1 order by id;
SELECT 10 - id FROM ((SELECT NULL AS col1, 1 AS id) UNION (SELECT '', 3) UNION (SELECT 'Cool',4) UNION (SELECT NULL AS col1, NULL AS id)) u1 order by id;
SELECT 10 + id FROM ((SELECT NULL AS col1, 1 AS id) UNION (SELECT '', 3) UNION (SELECT 'Cool',4) UNION (SELECT NULL AS col1, NULL AS id)) u1 order by id;
select * from names where lastname = (select ''::text);

-- Below queries are from tests/fixtures/TAS00.sql
SELECT ARRAY(SELECT '' FROM names);

-- Below queries are from tests/fixtures/TCEF03_noerrors_names.sql
SELECT GREATEST('');
SELECT LEAST('');

-- Below queries are from tests/fixtures/TVC01.sql
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

-- Test that IS '' and IS NOT '' are disallowed at the parser level even though '' is same as NULL in Octo
-- and IS NULL and IS NOT NULL are allowed.
select * from names where lastname is '';
select * from names where lastname is NOT '';

-- Test of various '' (i.e. empty string) handling code paths (Octo treats it as NULL whereas Postgres does not)
-- These queries were good test cases during an interim version of the PowerBI changes (YDBOcto#867) when NULL and ''
-- were distinguished by the types NUL_VALUE and NUL_UNKNOWN and there were bugs in the NUL_UNKNOWN handling. Since then,
-- NUL_UNKNOWN has been removed and merged into NUL_VALUE and so these tests are no longer relevant in the current code
-- but since having these tests does not hurt, we keep them.
select '' from names order by 1;
select t1.* = '' from names t1;
select '' = t1.* from names t1;
select t1.* = (select '') from names t1;
select (select '') = t1.* from names t1;
select n1.* > (select '') from names n1;
select n1.* < ANY (select '') from names n1;
select n1.* < ANY (select '' union select NULL) from names n1;
select n1.* < ANY (select NULL union select '') from names n1;
select n1.* NOT IN (select '' union select '') from names n1;
select n1.* similar to '' from names n1;
select '' similar to n1.* from names n1;
select greatest('','10','2');
select max('') from names;

