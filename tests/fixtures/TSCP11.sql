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

-- TABLENAME.ASTERISK with Group By and aggregate function COUNT
select count(n1.*) from ((select 1 as id, 'test' as name) union (select 2 as id, 'test' as name)) as n1;
select count(n1.*),n1.name from ((select 1 as id, 'test' as name) union (select 2 as id, 'test' as name)) as n1 group by n1.name;
select count(n1.id) from ((select 1 as id, 'test' as name) union (select 2 as id, 'test' as name)) as n1 group by n1.* having (count(n1.name)>1);
select count(n1.*) from ((select 1 as id, 'test' as name) union (select 2 as id, 'test' as name)) as n1 group by n1.* having (count(n1.*)>1);
select count(n1.*),n1.id from ((select 1 as id, 'test' as name) union (select 2 as id, 'test' as name)) as n1 group by n1.*,n1.id;
select count(n1.*),n1.id from ((select 1 as id, 'test' as name) union (select 2 as id, 'test' as name)) as n1 group by n1.*,n1.id having (n1.id > 1);
select count(n1.*),n1.id,n1.lastname from names n1 group by n1.*,n1.id,n1.lastname;
select count(n1.*) from ((select 1 as id, 'one' as lastname) union (select 1 as id, 'two' as lastname)) n1 group by n1.*;
select count(n1.*) from names n1 group by n1.* having (count(n1.*)>0);
select count(*) from ((select 1 as id, 'test' as name) union (select 2 as id, 'test' as name)) as n1 group by n1.* having (count(n1.name)>0);
select count(n2.fn) from ((select 1 as id, 'test' as fn) union all (select 2 as id, 'one' as fn)) n1, ((select 1 as id, 'hello' as fn) union all (select 2 as id, 'bello' as fn)) n2 group by n2.*;
select n1.* from names n1 group by n1.id,n1.firstname,n1.lastname;

-- Multiple use of TABLENAME.ASTERISK in Group By
select count(n1.*) from names n1 group by n1.*,n1.*;
select count(n1.*),count(n1.*) from names n1 group by n1.*,n1.*;
select count(n1.*),count(n1.*),n1.id from names n1 group by n1.*,n1.*,n1.id;
select count(n1.*),count(n1.*),n1.id from names n1 group by n1.*,n1.id;
select count(n1.*),count(n1.*),n1.id from names n1 group by n1.id;

-- Group by col1,col2-> group all the rows ( ex: r1 and r2) having corresponding same values for col1 and col2. i.e. r1.col1 == r2.col1 and r1.col2 == r2.col2
-- In a table like below
-- id fn
-- 1  hello
-- 2  bello
-- 1  hello
-- 2  bello
select count(n1.*) from ((select 1 as id, 'hello' as fn) union all (select 2 as id, 'bello' as fn) union all (select 1 as id, 'hello' as fn) union all (select 2 as id, 'bello' as fn)) n1 group by n1.*;
select count(n1.*) from ((select 1 as id, 'hello' as fn) union all (select 2 as id, 'bello' as fn) union all (select 1 as id, 'hello' as fn) union all (select 2 as id, 'bello' as fn) union all (select 1 as id, 'cat' as fn)) n1 group by n1.*;
select count(n1.*) from ((select 1 as id, 'hello' as fn) union all (select 2 as id, 'bello' as fn) union all (select 1 as id, 'hello' as fn) union all (select 2 as id, 'bello' as fn) union all (select 1 as id, 'cat' as fn)) n1 group by n1.id;

-- The following test the correctness of group_by_column_number usage in physical plan
select count(n1.*),n1.id,count(n1.*) from names n1 group by n1.id,n1.*,n1.*;
select count(n1.*),n1.id,count(n1.*) from names n1 group by n1.*,n1.id,n1.*;
select count(n1.*),n1.id,count(n1.*) from names n1 group by n1.*,n1.*,n1.id;
select count(n1.*),count(n1.*),n1.id from names n1 group by n1.*,n1.id,n1.*;
select count(n1.*),n1.firstname,count(n1.*) from names n1 group by n1.*,n1.firstname,n1.*;
select count(n1.*),count(n1.*),n1.lastname from names n1 group by n1.*,n1.lastname,n1.*;

-- DISINCT
select DISTINCT n1.* from names n1;
select DISTINCT n1.* from ((select 1 as id, 'hello' as fn) union all (select 2 as id, 'bello' as fn) union all (select 1 as id, 'hello' as fn) union all (select 2 as id, 'bello' as fn)) n1;
select DISTINCT count(n1.*) from ((select 1 as id, 'hello' as fn) union all (select 2 as id, 'bello' as fn) union all (select 1 as id, 'hello' as fn) union all (select 2 as id, 'bello' as fn)) n1;
select DISTINCT count(n1.*) from ((select 1 as id, 'hello' as fn) union all (select 2 as id, 'bello' as fn) union all (select 1 as id, 'hello' as fn) union all (select 2 as id, 'bello' as fn)) n1 group by n1.fn;
select count(DISTINCT n1.*) from names n1;
select count(DISTINCT tbl1.*) from (values (1,5), (2,4), (3,3), (4,2),(5,1)) as tbl1;
select count(DISTINCT n1.*) from names n1;
select count(DISTINCT n1.*) from (values (1,1,1), (1,1,2), (1,2,2), (2,1,1), (1,2,1), (2,1,2), (2,2,2), (2,2,1)) n1;
select count(DISTINCT n1.*) from ((select 1 as id, 'hello' as fn) union all (select 2 as id, 'bello' as fn)) n1;
select count(DISTINCT n1.*) from (select id,firstname from names) n1;
-- Generated by query generator and used to fail as NullRow was being counted
SELECT DISTINCT COUNT(alias1.*) FROM names AS names  LEFT OUTER JOIN names AS alias1 ON (('Lord' >= NULL::varchar) OR NOT (('Lord' != NULL::varchar) OR NOT (names.id > alias1.id))) WHERE (NOT (3 = names.id) AND NOT (names.lastName < 'Killer')) GROUP BY alias1.*, names.firstName;
-- Generated by query generator and is the reason why there is a check to see the key value is infact defined in `tmpl_table_asterisk`
SELECT DISTINCT COUNT(alias3.*) FROM names AS names  INNER JOIN (SELECT alias1.firstName, alias1.id, alias1.lastName FROM names alias1) AS alias1 ON ((names.firstName < alias1.firstName) AND NOT ((names.lastName < alias1.firstName)) AND NOT ((names.firstName < alias1.firstName))) INNER JOIN names AS alias3 ON (((names.firstName = SOME (SELECT ALL alias3.firstName FROM names alias3 ORDER BY alias3.firstName LIMIT 1)) OR NOT (names.firstName = 'Lord')) AND NOT ((names.lastName > ALL (SELECT DISTINCT alias4.firstName FROM names alias4 ORDER BY alias4.firstName LIMIT 1)))) RIGHT JOIN names AS alias6 ON ((names.firstName >= SOME (SELECT DISTINCT alias6.firstName FROM names alias6 ORDER BY alias6.firstName LIMIT 1)) AND NOT (names.lastName >= alias6.lastName)) LEFT OUTER JOIN names AS alias8 ON ((alias1.id >= ALL (SELECT ALL alias8.id FROM names alias8 ORDER BY alias8.id LIMIT 1)) OR ((alias1.id >= alias8.id) OR NOT (alias1.firstName >= SOME (SELECT DISTINCT alias9.firstName FROM names alias9 ORDER BY alias9.firstName LIMIT 1)))) WHERE (NOT ('Zero' = 'Acid') OR (NOT (names.firstName != names.firstName)) OR (names.lastName < names.lastName)) GROUP BY alias3.*, alias3.id HAVING alias3.id IN (2);

-- ALL
select count(ALL n1.*) from names n1;
select count(ALL tbl1.*) from (values (1,5), (2,4), (3,3), (4,2),(5,1)) as tbl1;
select count(ALL n1.*) from names n1;
select count(ALL n1.*) from (values (1,1,1), (1,1,2), (1,2,2), (2,1,1), (1,2,1), (2,1,2), (2,2,2), (2,2,1)) n1;
select count(ALL n1.*) from ((select 1 as id, 'hello' as fn) union all (select 2 as id, 'bello' as fn)) n1;
select count(n1.*) from (select id from names)n1;

-- VALUES
select count(tbl1.*) from (values (1,5), (2,4), (3,3), (4,2),(5,1)) as tbl1 group by tbl1.*;
SELECT count(n1.*) FROM (VALUES (0, 'Zero', 'Cool'), (1, 'Acid', 'Burn'), (2, 'Cereal', 'Killer'), (3, 'Lord', 'Nikon'), (4, 'Joey', NULL), (5, 'Zero', 'Cool'))n1;
SELECT count(n1.*) FROM (VALUES (0, 'Zero', 'Cool'), (1, 'Acid', 'Burn'), (2, 'Cereal', 'Killer'), (3, 'Lord', 'Nikon'), (4, 'Joey', NULL), (5, 'Zero', 'Cool'))n1 group by n1.*;
SELECT abcd.* FROM (VALUES ((SELECT 1), (SELECT 2), 3)) as abcd;
SELECT count(abcd.*) FROM (VALUES ((SELECT 1), (SELECT 2), 3)) as abcd group by abcd.*;
SELECT count(n1.*) FROM (VALUES (1)) n1 INNER JOIN (VALUES (2)) n2 ON n1.column1 < n2.column1;
SELECT count(n1.*) FROM (values (1,5), (2,4), (3,3), (4,2),(5,1)) n1 INNER JOIN (values (1,5), (2,4), (3,3), (4,2),(5,1)) n2 ON n1.column1 < n2.column1 group by n1.*;
SELECT DISTINCT COUNT(alias1.*) FROM (VALUES (0, 'Zero', 'Cool'), (1, 'Acid', 'Burn'), (2, 'Cereal', 'Killer'), (3, 'Lord', 'Nikon'), (4, 'Joey', NULL), (5, 'Zero', 'Cool')) AS names  LEFT OUTER JOIN names AS alias1 ON (('Lord' >= NULL::varchar) OR NOT (('Lord' != NULL::varchar) OR NOT (names.column1 > alias1.id))) WHERE (NOT (3 = names.column1) AND NOT (names.column3 < 'Killer')) GROUP BY alias1.*, names.column2;
SELECT alias1.column2,COUNT(alias1.*) FROM (VALUES (9, 'Zero', 'Cool'), (8, 'Acid', 'Burn'), (7, 'Cereal', 'Killer'), (6, 'Lord', 'Nikon')) as orders LEFT JOIN (VALUES (0, 'Zero', 'Cool'), (1, 'Acid', 'Burn'), (2, 'Cereal', 'Killer'), (3, 'Lord', 'Nikon'), (4, 'Joey', NULL), (5, 'Zero', 'Cool')) AS alias1 ON (((orders.column1 = alias1.column1)) AND NOT (orders.column2 = alias1.column2)) GROUP BY alias1.*, alias1.column2 ORDER BY COUNT(alias1.*), alias1.column2;

-- Following queries verify that similar tablenames are processed correctly
-- Refers to https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/816#note_533892613
select count(*) from (select 1 from names n1, names n11 group by n1.*, n11.*) tbl1;
select count(*) from (select 1 from names n11, names n1 group by n1.*, n11.*) tbl1;

-- Testing count(table.*) with single column tables
select count(DISTINCT n1.*) from ((select 'hello' as fn) union all (select 'bello' as fn)) n1;
select count(DISTINCT n1.*) from ((select 1 as id) union all (select 2 as id)) n1;
select count(DISTINCT tbl1.*) from (values (1), (2), (3), (4),(5)) as tbl1;
select count(tbl1.*) from (values (1), (2), (3), (4),(5)) as tbl1 group by tbl1.*;
select count(n1.*) from (select firstname from names)n1;
select count(DISTINCT n1.*) from (select firstname from names)n1;

-- Testing table.* usage when few rows have all NULL column values
select count(distinct n2.*) from (values (1, 'abcd'), (2, NULL), (NULL, 'efgh'), (NULL, NULL)) n2;
select count(distinct n2.*) from (values (1, 'abcd'), (2, NULL), (NULL, 'efgh'), (NULL, NULL), (NULL, NULL)) n2;
select count(n2.*) from (values (1, 'abcd'), (2, NULL), (NULL, 'efgh'), (NULL, NULL)) n2;
select count(n2.*) from (values (1, 'abcd'), (2, NULL), (NULL, 'efgh'), (NULL, NULL), (NULL, NULL)) n2;
select count(distinct n2.*) from (values (1, 'abcd'), (2, NULL), (NULL, 'efgh'), (NULL, NULL)) n2 group by n2.*;
select count(n2.*) from (values (1, 'abcd'), (2, NULL), (NULL, 'efgh'), (NULL, NULL)) n2 group by n2.*;

-- Testing table.* and * usage
select * from names n3 where (1 = (select id from (select * from (select n3.* from names n1) n4) n2 limit 1));
select * from names n3 where exists (select * from (select n3.* from names n1) n2);
select * from names n3 inner join names n4 on exists (select * from (select n3.* from names n1) n2);
select * from names n3 left join names n4 on exists (select * from (select n3.* from names n1) n2);
select * from names n3 right join names n4 on exists (select * from (select n3.* from names n1) n2);
-- Below query is commented out because Postgres issues an error (FULL JOIN not supported)
-- select * from names n3 full join names n4 on exists (select * from (select n3.* from names n1));
select count(*) from names n4 group by firstname having exists (select * from names n3 where exists (select * from (select n3.* from names n1) n2));
select count(*) from names n4 group by n4.* having exists (select * from names n3 where exists (select * from (select n3.* from names n1) n2));
select * from (select n2.* from (select n1.* from names n1) n2) n3;
select * from (select * from (select n1.* from names n1) n2) n3;
select * from (select n3.* from (select * from (select n1.* from names n1) n2) n3) n4;
select * from (select n1.*,n1.id from names n1) n2;

-- Natural join common column case
SELECT * FROM (SELECT names.* FROM names) n1 NATURAL JOIN (SELECT names.* FROM names) n2;

-- Correlation specification of columns with table.* usage
select a,b,c from (select n1.* from names n1) as abcd(a,b,c);
