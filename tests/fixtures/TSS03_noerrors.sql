#################################################################
#								#
# Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- All queries in this query file are valid queries that do not issue any error.

-- TSS03 : OCTO192 : Using sub-query in a WHERE clause produces <Plan produced by optimizer appears incorrect> warning

-- Sub-query that returns 1 row, 1 column (i.e. a scalar) and type match
SELECT * FROM names a WHERE a.firstName = (SELECT b.firstName FROM names b WHERE a.id = b.id);
SELECT * FROM names a WHERE a.firstName != (SELECT b.firstName FROM names b WHERE a.id = b.id);

-- Misc queries
SELECT * FROM names n1 WHERE n1.firstName = (SELECT n2.firstname FROM names n2 WHERE n2.firstname = n1.firstname limit 1);
SELECT * FROM names n1 WHERE n1.firstName = (SELECT n2.firstname FROM names n2 WHERE n2.firstname != n1.firstname limit 1);
SELECT * FROM names n1 WHERE 'Zero' = (SELECT n2.firstname FROM names n2 WHERE n2.firstname != n1.firstname limit 1);
SELECT * FROM names n1 WHERE n1.firstName IN (SELECT n2.firstname FROM names n2 WHERE n2.firstname = n1.firstname limit 1);

-- Sub-query that returns 1 row, 1 column (i.e. a scalar) and type match. Use a sub-query wherever a scalar is possible
select * from names where id = 1+(select 1 from names limit 1);
select (case when id > (select 1) then (select 1) else (select 0) end)-1 as idbool from names;
select (select 1 from names limit 1)%(select 2 from names limit 1);
select (select 1 from names limit 1)*(select 2 from names limit 1)/2;
select (select 1 from names limit 1)-(select 2 from names limit 1)+3;
select 4/(select 2 from names limit 1)-3;
select (select 1 from names limit 1)*(select 2 from names limit 1)*3;
select 3%(select 2 from names limit 1)+3;
select id+(select 2 from names limit 1) from names;
select id*(select 2 from names limit 1) from (select id from names) as n2;
select (case when id > (select 1 from names limit 1) then 1 else 0 end)+1 as idbool from names;
select (select 1 from names limit 1)*(case when id > 1 then 1 else 0 end) as idbool from names;
select id-(id*(select 2 from names limit 1)) from names;
select (select firstname from names limit 1) || (select lastname from names limit 1);

-- Sub-query that returns 0 rows
SELECT * FROM names a WHERE a.firstName = (SELECT b.firstName FROM names b WHERE -a.id = b.id);

-- Sub-query that returns a scalar NULL (== 1 row, == 1 column)
SELECT * FROM names a WHERE a.firstName = (SELECT NULL::VARCHAR);

