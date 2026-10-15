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

-- TOB23 : OCTO1161 : ORDER BY on a key column is optimized when the key columns before it are fixed to one value

-- The key column of the first table is fixed to one value, so the second table's key loop gives the output order.
select n1.id, n2.id from names n1, names n2 where n1.id = 2 order by n2.id desc limit 3;

-- LEFT JOIN where the key column of the first table is fixed to one value in the WHERE clause
select n1.id, n2.id from names n1 left join names n2 on n2.id > n1.id where n1.id = 2 order by n2.id desc;
select n1.id, n2.firstname from names n1 left join names n2 on n2.id < n1.id where n1.id = 3 order by n2.id;

-- ORDER BY optimization does not take effect with a RIGHT JOIN
select n1.id, n2.id from names n1 right join names n2 on n1.id = 2 order by n2.id desc;
