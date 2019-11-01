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

-- ---------------------
-- Below is a list of queries using sub-queries.
-- It is a mix of valid and invalid queries and this test checks the behavior of Octo in both cases.
-- ---------------------
-- Sub-query that returns 1 row, 1 column (i.e. a scalar) and type match
SELECT * FROM names a WHERE a.firstName = (SELECT b.firstName FROM names b WHERE a.id = b.id);
SELECT * FROM names a WHERE a.firstName != (SELECT b.firstName FROM names b WHERE a.id = b.id);
-- Misc queries
SELECT * FROM names n1 WHERE n1.firstName = (SELECT n2.firstname FROM names n2 WHERE n2.firstname = n1.firstname);
SELECT * FROM names n1 WHERE n1.firstName = (SELECT n2.firstname FROM names n2 WHERE n2.firstname = n1.firstname limit 1);
SELECT * FROM names n1 WHERE n1.firstName = (SELECT n2.firstname FROM names n2 WHERE n2.firstname != n1.firstname limit 1);
SELECT * FROM names n1 WHERE 'Zero' = (SELECT n2.firstname FROM names n2 WHERE n2.firstname != n1.firstname limit 1);
SELECT * FROM names n1 WHERE n1.firstName IN (SELECT n2.firstname FROM names n2 WHERE n2.firstname = n1.firstname limit 1);
SELECT * from names n1 where n1.id IN (SELECT * FROM names n2 WHERE n2.id = ((n1.id + 1) % 6));
-- Sub-query that returns 1 row, 1 column (i.e. a scalar) and type match. Use a sub-query wherever a scalar is possible
select * from names where id = 1+(select 1 from names limit 1);
select (case when id > (select 1) then (select 1) else (select 0) end)-1 as idbool from names;
select (select 1 from names limit 1)%(select 2 from names limit 1);
select (select 1 from names limit 1)*(select 2 from names limit 1)/3;
select (select 1 from names limit 1)-(select 2 from names limit 1)+3;
select 4/(select 2 from names limit 1)-3;
select (select 1 from names limit 1)*(select 2 from names limit 1)*3;
select 3%(select 2 from names limit 1)+3;
select id+(select 2 from names limit 1) from names;
select id*(select 2 from names limit 1) from (select id from names) as n2;
select id/(select 2 from names) from (select id from names) as n2;
select 2*(select id*id from names) from (select id from names) as n2;
select 2+(select * from ((select id from names) union (select id from names)) as n1 limit 1) from (select id from names) as n2;
select 2-(select * from ((select id from names) union (select 2::integer from names)) as n1 limit 1) from (select id from names) as n2;
select (case when id > (select 1 from names limit 1) then 1 else 0 end)+1 as idbool from names;
select (select 1 from names limit 1)*(case when id > 1 then 1 else 0 end) as idbool from names;
select id-(id*(select 2 from names limit 1)) from names;
select (select firstname from names limit 1) || (select lastname from names limit 1);
-- Sub-query that returns 1 row, 1 column (i.e. a scalar) and type mismatch
SELECT * FROM names a WHERE a.firstName = (SELECT b.id FROM names b WHERE a.id = b.id);
SELECT * FROM names a WHERE a.firstName != (SELECT b.id FROM names b WHERE a.id = b.id);
-- Sub-query that returns 0 rows
SELECT * FROM names a WHERE a.firstName = (SELECT b.firstName FROM names b WHERE -a.id = b.id);
-- Sub-query that returns 1 row but multiple columns
SELECT * FROM names a WHERE a.firstName = (SELECT b.firstName,b.lastname FROM names b WHERE a.id = b.id);
SELECT * FROM names a WHERE a.firstName != (SELECT b.firstName,b.lastname FROM names b WHERE a.id = b.id);
-- Sub-query that returns > 1 rows but == 1 column
SELECT * FROM names a WHERE a.firstName = (SELECT b.firstName FROM names b WHERE (a.id % 2) = (b.id % 2));
SELECT * FROM names a WHERE a.firstName != (SELECT b.firstName FROM names b WHERE (a.id % 2) = (b.id % 2));
-- Sub-query that returns > 1 rows and  > 1 columns
SELECT * FROM names a WHERE a.firstName = (SELECT b.firstName,b.lastname FROM names b WHERE (a.id % 2) = (b.id % 2));
SELECT * FROM names a WHERE a.firstName != (SELECT b.firstName,b.lastname FROM names b WHERE (a.id % 2) = (b.id % 2));
-- Sub-query that returns a scalar NULL (== 1 row, == 1 column)
SELECT * FROM names a WHERE a.firstName = (SELECT NULL);
-- Sub-query that returns a NULL in == 1 row, > 1 columns
SELECT * FROM names a WHERE a.firstName = (SELECT NULL, NULL, firstname);
SELECT * FROM names a WHERE a.firstName != (SELECT NULL, NULL, firstname);
-- Sub-query that returns a NULL in  > 1 row, > 1 columns
SELECT * FROM names n1 WHERE n1.firstName = (SELECT NULL, n2.id, NULL from names n2);
SELECT * FROM names n1 WHERE n1.firstName != (SELECT NULL, n2.id, NULL from names n2);
-- Miscellaneous queries that issue error but used to assert fail due to an incorrect change to match_column_in_table.c
select id+2;
