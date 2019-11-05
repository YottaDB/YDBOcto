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

-- outer join with matching rows is the same as inner join
SELECT * FROM names n1 LEFT JOIN names n2 ON n1.id = n2.id;
SELECT * FROM names n1 INNER JOIN names n2 ON n1.id = n2.id;
-- outer join followed by a SET operation
SELECT n1.id,n1.firstname,n2.lastname FROM names n1 LEFT JOIN names n2 ON n1.id = n2.id UNION ALL SELECT * FROM names n1;
SELECT n2.id,n2.firstname,n1.lastname FROM names n1 INNER JOIN names n2 ON n1.id = n2.id UNION ALL SELECT * FROM names n1;
-- left outer join with missing columns
SELECT * FROM names n1 LEFT JOIN (SELECT * FROM names WHERE firstName < 'Zero') n2 ON n1.id = n2.id;
-- right outer join with matching rows is the same as inner join
SELECT * FROM names n1 RIGHT JOIN names n2 ON n1.id = n2.id;
SELECT * FROM names n1 INNER JOIN names n2 ON n1.id = n2.id;
-- right outer join followed by a SET operation
SELECT n1.id,n1.firstname,n2.lastname FROM names n1 RIGHT JOIN names n2 ON n1.id = n2.id UNION ALL SELECT * FROM names n1;
SELECT n2.id,n2.firstname,n1.lastname FROM names n1 INNER JOIN names n2 ON n1.id = n2.id UNION ALL SELECT * FROM names n1;
-- right outer join with missing columns
SELECT * FROM (SELECT * FROM names WHERE firstName < 'Zero') n1 RIGHT JOIN names n2 ON n1.id = n2.id;
-- full outer join with missing columns
SELECT * FROM (SELECT * FROM names WHERE firstName = 'Zero') n1 FULL OUTER JOIN (SELECT * FROM names WHERE firstName = 'Acid') n2 ON n1.id = n2.id;
-- nested select with outer join
SELECT * FROM (SELECT n1.id,n1.firstname as n1f,n1.lastname,n2.id,n2.firstname,n2.lastname FROM names n1 LEFT JOIN names n2 ON n1.id = n2.id) query1 WHERE n1f :: text = 'Zero';
-- Below are queries to test #363
select n1.id,n2.id from names n1 full  join names n2 on  1 = 0;
select n1.id,n2.id from names n1 full  join names n2 on  1 = 0 and n1.id = n2.id;
select n1.id,n2.id from names n1 left join names n2 on 1 = 0;
select n1.id,n2.id from names n1 left join names n2 on 1 = 0 and n1.id = n2.id;
select n1.id,n2.id from names n1 right join names n2 on 1 = 0;
select n1.id,n2.id from names n1 right join names n2 on 1 = 0 and n1.id = n2.id;
-- Below are queries from #361 that could not be included in TSS04.sql because they also needed #363 to be fixed
select n1.id,n2.id from names n1 full  join names n2 on n1.id = n2.id AND EXISTS (select * from names);
select n1.id,n2.id from names n1 full  join names n2 on n1.id = n2.id AND NOT EXISTS (select * from names);
