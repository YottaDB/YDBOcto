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

-- Below section of tests should be enabled when #311 is fixed
-- -- Tests that were added for #192 but failed because they were waiting for #311 to be fixed.
-- -- Misc queries that will not work until $ZYSQLNULL (#311) is fixed
-- SELECT * FROM names a WHERE a.firstName != (SELECT b.firstName FROM names b WHERE a.id = (b.id + 2));
-- SELECT * FROM names a WHERE a.firstName != (SELECT b.firstName FROM names b WHERE -a.id = b.id);
-- SELECT * FROM names a WHERE a.firstName != (SELECT NULL);
-- -- Sub-query that returns a NULL ( > 1 row, == 1 column)
-- SELECT * FROM names a WHERE a.firstName = (SELECT NULL from names limit 2);
-- SELECT * FROM names a WHERE a.firstName != (SELECT NULL from names limit 2);
--
-- SELECT AVG(id),col1 from ((select NULL as col1, 1 as id) union (select 'First', 2) union (select '', 3) union (select 'Cool',4) union (select 'Cool',5)) u1 GROUP BY col1 order by col1;

