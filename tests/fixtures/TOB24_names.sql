#################################################################
#								#
# Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TOB24 : OCTO1160 : ORDER BY on a non-key column walks an AIM cross reference of that column

-- VARCHAR column with a NULL value (names.lastname of id 4). NULLs come last in ascending order and first in descending order.
select * from names order by lastname, id;
select * from names order by lastname desc, id;
select * from names order by lastname desc, id desc;
select * from names order by lastname limit 1;
select * from names order by lastname desc limit 1;
select * from names order by firstname, id limit 2;
select * from names order by firstname desc, id limit 3;
select lastname is null, lastname, id from names order by lastname desc, id;

-- ORDER BY a select column alias of a non-key column (and of the key column) is the same as ORDER BY that column
select id, lastname as ln from names order by ln, id;
select id as nid, lastname from names order by lastname, nid;

-- The key column of the first table is fixed to one value, so the second table's cross reference walk gives the output order
select n1.id, n2.lastname, n2.id from names n1, names n2 where n1.id = 2 order by n2.lastname desc, n2.id;

-- WHERE with a range or a LIKE prefix on the ORDER BY column. The cross reference walk of the WHERE gives the output order.
select * from names where lastname > 'C' order by lastname desc, id;
select * from names where lastname >= 'Cool' order by lastname desc limit 2;
select * from names where lastname < 'Killer' order by lastname, id;
select * from names where lastname <= 'Killer' order by lastname desc, id desc;
select * from names where firstname like 'Ze%' order by firstname, id;
-- A LIKE prefix in descending order walks down from just after the last value that starts with the prefix
select * from names where firstname like 'Ze%' order by firstname desc, id;
select * from names where lastname like 'Cool%' order by lastname desc, id desc;
select * from names where lastname like 'K%' order by lastname desc limit 1;
select * from names where lastname like 'Zz%' order by lastname desc, id;
-- A WHERE "=" on the ORDER BY column: that column orders on a value that does not change
select * from names where firstname = 'Zero' order by firstname, id;

-- ORDER BY optimization does not take effect
-- 1) The second ORDER BY column is not the primary key column
select * from names order by lastname, firstname;
-- 2) More ORDER BY columns than primary key columns
select * from names order by lastname, id, firstname;
-- 3) WHERE on another column already chose how to walk the table
select * from names where firstname = 'Zero' order by lastname, id;
-- 4) WHERE on the primary key column
select * from names where id > 1 order by lastname, id;
-- 5) GROUP BY
select lastname, count(*) from names group by lastname order by lastname;
-- 6) RIGHT JOIN
select n1.id, n2.lastname from names n1 right join names n2 on n1.id = n2.id order by n2.lastname, n2.id;
-- 7) ORDER BY "table.*", which is not one column (first and second ORDER BY item)
select * from names order by names.*;
select * from names order by lastname, names.*;
