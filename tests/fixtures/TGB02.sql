#################################################################
#								#
# Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TGB02 : OCTO55 : GROUP BY and AGGREGATE FUNCTIONS : Various valid scenarios in NAMES schema

--> Below are examples of GROUP BY without any aggregate function use.
SELECT firstname FROM names GROUP BY firstname;
SELECT lastname  FROM names GROUP BY lastname;
SELECT firstname,lastname FROM names GROUP BY firstname,lastname;
SELECT 'abcd' || firstname FROM names GROUP BY firstname;
SELECT 'abcd' FROM names GROUP BY firstname;
SELECT 'efgh' FROM names GROUP BY lastname;
SELECT 'ijkl' FROM names GROUP BY id;
SELECT firstname FROM names n1 WHERE n1.id < 4 OR n1.id > 1 GROUP BY firstname;
SELECT firstname FROM names n1 WHERE n1.id < 3 OR n1.id > 3 GROUP BY firstname;

--> Below are examples of GROUP BY with aggregate function use
SELECT firstname,COUNT(firstname) FROM names GROUP BY firstname;
SELECT firstname,COUNT('abcd' || firstname),2+3*MAX(2*id+3),MIN(lastname) FROM names GROUP BY firstname;
SELECT firstname,MIN(firstname) FROM names GROUP BY firstname;
SELECT firstname,MAX(firstname) FROM names GROUP BY firstname;
SELECT firstname,MIN(lastname) FROM names GROUP BY firstname;
SELECT firstname,MAX(lastname) FROM names GROUP BY firstname;
SELECT firstname,COUNT(*) FROM names GROUP BY firstname;
SELECT firstname,COUNT(id) FROM names GROUP BY firstname;
SELECT firstname,MIN(id) FROM names GROUP BY firstname;
SELECT firstname,MAX(id) FROM names GROUP BY firstname;
SELECT firstname,SUM(id) FROM names GROUP BY firstname;
SELECT firstname,AVG(id) FROM names GROUP BY firstname;
SELECT firstname,COUNT(DISTINCT firstname) FROM names GROUP BY firstname;
SELECT firstname,MIN(DISTINCT firstname) FROM names GROUP BY firstname;
SELECT firstname,MAX(DISTINCT firstname) FROM names GROUP BY firstname;
SELECT firstname,MIN(DISTINCT lastname) FROM names GROUP BY firstname;
SELECT firstname,MAX(DISTINCT lastname) FROM names GROUP BY firstname;
SELECT firstname,COUNT(DISTINCT id) FROM names GROUP BY firstname;
SELECT firstname,MIN(DISTINCT id) FROM names GROUP BY firstname;
SELECT firstname,MAX(DISTINCT id) FROM names GROUP BY firstname;
SELECT firstname,SUM(DISTINCT id) FROM names GROUP BY firstname;
SELECT firstname,AVG(DISTINCT id) FROM names GROUP BY firstname;
SELECT MAX((n2.id % 3) + 4) FROM names n2 GROUP BY n2.firstname;

--> Below are examples of GROUP BY with aggregate function use on a column derived from a sub-query
SELECT firstname,COUNT(firstname) FROM (select * from names n1 UNION ALL select * from names n2) n3 GROUP BY firstname;
SELECT firstname,MIN(firstname) FROM (select * from names n1 UNION ALL select * from names n2) n3 GROUP BY firstname;
SELECT firstname,MAX(firstname) FROM (select * from names n1 UNION ALL select * from names n2) n3 GROUP BY firstname;
SELECT firstname,MIN(lastname) FROM (select * from names n1 UNION ALL select * from names n2) n3 GROUP BY firstname;
SELECT firstname,MAX(lastname) FROM (select * from names n1 UNION ALL select * from names n2) n3 GROUP BY firstname;
SELECT firstname,COUNT(*) FROM (select * from names n1 UNION ALL select * from names n2) n3 GROUP BY firstname;
SELECT firstname,COUNT(id) FROM (select * from names n1 UNION ALL select * from names n2) n3 GROUP BY firstname;
SELECT firstname,MIN(id) FROM (select * from names n1 UNION ALL select * from names n2) n3 GROUP BY firstname;
SELECT firstname,MAX(id) FROM (select * from names n1 UNION ALL select * from names n2) n3 GROUP BY firstname;
SELECT firstname,SUM(id) FROM (select * from names n1 UNION ALL select * from names n2) n3 GROUP BY firstname;
SELECT firstname,AVG(id) FROM (select * from names n1 UNION ALL select * from names n2) n3 GROUP BY firstname;
SELECT firstname,COUNT(DISTINCT firstname) FROM (select * from names n1 UNION ALL select * from names n2) n3 GROUP BY firstname;
SELECT firstname,MIN(DISTINCT firstname) FROM (select * from names n1 UNION ALL select * from names n2) n3 GROUP BY firstname;
SELECT firstname,MAX(DISTINCT firstname) FROM (select * from names n1 UNION ALL select * from names n2) n3 GROUP BY firstname;
SELECT firstname,MIN(DISTINCT lastname) FROM (select * from names n1 UNION ALL select * from names n2) n3 GROUP BY firstname;
SELECT firstname,MAX(DISTINCT lastname) FROM (select * from names n1 UNION ALL select * from names n2) n3 GROUP BY firstname;
SELECT firstname,COUNT(DISTINCT id) FROM (select * from names n1 UNION ALL select * from names n2) n3 GROUP BY firstname;
SELECT firstname,MIN(DISTINCT id) FROM (select * from names n1 UNION ALL select * from names n2) n3 GROUP BY firstname;
SELECT firstname,MAX(DISTINCT id) FROM (select * from names n1 UNION ALL select * from names n2) n3 GROUP BY firstname;
SELECT firstname,SUM(DISTINCT id) FROM (select * from names n1 UNION ALL select * from names n2) n3 GROUP BY firstname;
SELECT firstname,AVG(DISTINCT id) FROM (select * from names n1 UNION ALL select * from names n2) n3 GROUP BY firstname;

--> Miscellaneous queries that do not fit in any section but were considered interesting at some point in time during development
SELECT firstname,COUNT(1+id),MIN(firstname) FROM names GROUP BY firstname;
select * from (select lastname || firstname as lastfirst, firstname || lastname as firstlast from (select * from names) n1) n2;

--> Below are examples of aggregate function use without GROUP BY
SELECT COUNT(*) FROM names;
SELECT COUNT(id) FROM names;
SELECT COUNT(firstname) FROM names;
SELECT COUNT(2) FROM names;
SELECT COUNT(2.5) FROM names;
SELECT COUNT('abcd') FROM names;
select 2+COUNT(id) from names;
SELECT AVG(1+2*id) FROM names;
SELECT 1+2*AVG(id) FROM names;
SELECT 1+2*AVG(id)+COUNT(firstname) FROM names;

--> Below should not error out because * (all columns) is in select list and all those column names are also in the group by list
SELECT * FROM names GROUP BY id,firstname,lastname;
SELECT id,firstname,lastname FROM names GROUP BY id,firstname,lastname;

--> Test ORDER BY with GROUP BY
SELECT firstname FROM names GROUP BY firstname ORDER BY firstname DESC;
SELECT AVG(2*id+2*id),firstname FROM names GROUP BY firstname ORDER BY 1;

--> Test GROUP BY in sub-queries
SELECT * FROM (SELECT firstname,COUNT(id) FROM names n1 GROUP BY firstname) n2;
SELECT * FROM names n0 WHERE n0.id IN (SELECT COUNT(n1.id) FROM names n1 GROUP BY n1.firstname);
SELECT COUNT(n1.id),n1.firstname FROM names n1 WHERE n1.id IN (SELECT MAX(n2.id) FROM names n2 GROUP BY n2.firstname) GROUP BY n1.firstname;
SELECT 1 + COUNT(n1.id * 2),n1.firstname FROM names n1 where n1.id IN (SELECT MAX((n2.id % 3) + 4) FROM names n2 GROUP BY n2.firstname) GROUP BY n1.firstname ORDER BY 2;
SELECT COUNT(n1.id),n1.firstname FROM names n1 WHERE EXISTS (SELECT MAX(n2.id) FROM names n2 WHERE n2.id < n1.id GROUP BY n2.firstname) GROUP BY n1.firstname;

--> Test GROUP BY in sub-queries using one or more columns from outer queries
SELECT id,firstname FROM names n1 WHERE id IN (SELECT MAX(n2.id) FROM names n2 GROUP BY n1.id);
SELECT id,firstname FROM names n1 WHERE id IN (SELECT MAX(n2.id) FROM names n2 GROUP BY n2.id,n1.id);
SELECT id,firstname FROM names n1 WHERE id IN (SELECT MAX(n2.id) FROM names n2 GROUP BY n1.id,n2.id);
SELECT id,firstname FROM names n1 WHERE id IN (SELECT MAX(n2.id) FROM names n2 GROUP BY n1.id,n2.id,n1.id);
SELECT id,firstname FROM names n1 WHERE id < (SELECT MAX(n2.id) FROM names n2 GROUP BY n1.id);
SELECT id,firstname FROM names n1 where id IN (SELECT n1.id FROM names GROUP BY n1.id);
SELECT id,firstname FROM names n1 where id IN (SELECT MAX(n2.id) FROM names n2 GROUP BY n1.id);
SELECT id,firstname FROM names n1 where id IN (SELECT COUNT(n2.id) FROM names n2 GROUP BY n1.id);
SELECT id,firstname FROM names n1 where id IN (SELECT n1.id FROM names GROUP BY n1.firstname);
SELECT id,firstname FROM names n1 where id IN (SELECT COUNT(n2.id) FROM names n2 GROUP BY n1.firstname,n2.id);

--> Test aggregate functions in sub-queries without GROUP BY
SELECT * FROM names n1 WHERE n1.firstName IN (SELECT MAX(n2.firstname) FROM names n2);

--> Test that GROUP BY with OR clause (which is optimized to DNF/Disjunctive-Normal-Form) does not cause duplicates (id = 3 matches id < 4 and id > 2)
SELECT firstname,COUNT(id) FROM names WHERE id < 4 OR id > 2 GROUP BY firstname;

--> Test SELECT DISTINCT with GROUP BY
SELECT DISTINCT 1 FROM names n1 WHERE n1.id < 4 OR n1.id > 1 GROUP BY firstname;
SELECT DISTINCT firstname FROM names n1 WHERE n1.id < 4 OR n1.id > 1 GROUP BY firstname;
SELECT DISTINCT MAX((n2.id % 3) + 4) FROM names n2 GROUP BY n2.firstname;
SELECT DISTINCT 1 + COUNT(n1.id * 2),n1.firstname FROM names n1 WHERE n1.id IN (SELECT DISTINCT MAX((n2.id % 3) + 4) FROM names n2 GROUP BY n2.firstname) GROUP BY n1.firstname ORDER BY 2, 1;

--> Test LIMIT with GROUP BY
--> Note: Need to add `rowcount-only-check` as a comment below since Postgres and Octo output might vary with LIMIT.
-->	  See `run_query_in_octo_and_postgres_and_crosscheck()` in `test_helpers.bash.in` for details.
SELECT firstname FROM names GROUP BY firstname LIMIT 0;
SELECT firstname FROM names GROUP BY firstname LIMIT 1;				-- rowcount-only-check
SELECT firstname FROM names GROUP BY firstname LIMIT 2;				-- rowcount-only-check
SELECT firstname FROM names GROUP BY firstname LIMIT 3;				-- rowcount-only-check
SELECT firstname FROM names GROUP BY firstname LIMIT 7;				-- rowcount-only-check
SELECT firstname FROM names GROUP BY firstname ORDER BY firstname LIMIT 0;
SELECT firstname FROM names GROUP BY firstname ORDER BY firstname LIMIT 1;	-- rowcount-only-check
SELECT firstname FROM names GROUP BY firstname ORDER BY firstname LIMIT 2;	-- rowcount-only-check
SELECT firstname FROM names GROUP BY firstname ORDER BY firstname LIMIT 6;	-- rowcount-only-check
SELECT firstname FROM names GROUP BY firstname ORDER BY firstname LIMIT 7;	-- rowcount-only-check

--> Test queries that are identical except for different GROUP BY column order
SELECT COUNT(2*id),firstname FROM names GROUP BY lastname,firstname,id;
SELECT COUNT(2*id),firstname FROM names GROUP BY firstname,id,lastname;
SELECT COUNT(2*id),firstname FROM names GROUP BY id,lastname,firstname;
SELECT * FROM names GROUP BY lastname,firstname,id;
SELECT * FROM names GROUP BY firstname,id,lastname;
SELECT * FROM names GROUP BY id,lastname,firstname;

--> Below should not error out (even though COUNT aggregate function call is nested, the nested call corresponds to a sub-query)
SELECT 1+COUNT(n1.id+(SELECT COUNT(n2.id) from names n2 WHERE n1.id > (n2.id + 2))) from names n1;

