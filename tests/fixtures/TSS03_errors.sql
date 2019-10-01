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

-- All queries in this query file are invalid queries and generate an error.

-- TSS03 : OCTO192 : Using sub-query in a WHERE clause produces <Plan produced by optimizer appears incorrect> warning

-- Misc queries
SELECT * FROM names n1 WHERE n1.firstName = (SELECT n2.firstname FROM names n2 WHERE n2.firstname = n1.firstname);
SELECT * from names n1 where n1.id IN (SELECT * FROM names n2 WHERE n2.id = ((n1.id + 1) % 6));

-- Sub-query that returns 1 row, 1 column (i.e. a scalar) and type match. Use a sub-query wherever a scalar is possible
select id/(select 2 from names) from (select id from names) as n2;
select 2*(select id*id from names) from (select id from names) as n2;

-- Sub-query that returns 1 row, 1 column (i.e. a scalar) and type mismatch
SELECT * FROM names a WHERE a.firstName = (SELECT b.id FROM names b WHERE a.id = b.id);
SELECT * FROM names a WHERE a.firstName != (SELECT b.id FROM names b WHERE a.id = b.id);

-- Sub-query that returns 1 row but multiple columns
SELECT * FROM names a WHERE a.firstName = (SELECT b.firstName,b.lastname FROM names b WHERE a.id = b.id);
SELECT * FROM names a WHERE a.firstName != (SELECT b.firstName,b.lastname FROM names b WHERE a.id = b.id);

-- Sub-query that returns > 1 rows but == 1 column
SELECT * FROM names a WHERE a.firstName = (SELECT b.firstName FROM names b WHERE (a.id % 2) = (b.id % 2));
SELECT * FROM names a WHERE a.firstName != (SELECT b.firstName FROM names b WHERE (a.id % 2) = (b.id % 2));

-- Sub-query that returns > 1 rows and  > 1 columns
SELECT * FROM names a WHERE a.firstName = (SELECT b.firstName,b.lastname FROM names b WHERE (a.id % 2) = (b.id % 2));
SELECT * FROM names a WHERE a.firstName != (SELECT b.firstName,b.lastname FROM names b WHERE (a.id % 2) = (b.id % 2));

-- Sub-query that returns a NULL in == 1 row, > 1 columns
SELECT * FROM names a WHERE a.firstName = (SELECT NULL, NULL, firstname);
SELECT * FROM names a WHERE a.firstName != (SELECT NULL, NULL, firstname);

-- Sub-query that returns a NULL in  > 1 row, > 1 columns
SELECT * FROM names n1 WHERE n1.firstName = (SELECT NULL, n2.id, NULL from names n2);
SELECT * FROM names n1 WHERE n1.firstName != (SELECT NULL, n2.id, NULL from names n2);

-- Miscellaneous queries that issue error but used to assert fail due to an incorrect change to match_column_in_table.c
select id+2;

