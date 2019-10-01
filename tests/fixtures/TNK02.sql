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

-- TNK02 : OCTO443 : Multiple OUTER JOIN returns incorrect results

-- Test with LEFT JOIN
select n1.id,n2.id,n3.id from names n1 left join names n2 on (n1.firstname = 'abcd') left join names n3 on (n2.firstname = n3.lastname);

-- Test with RIGHT JOIN
SELECT n1.id as n1id, n2.id as n2id, n3.id as n3id FROM names n1 RIGHT OUTER JOIN names n2 ON (n1.firstname = 'abcd') RIGHT JOIN names n3 ON (n1.firstname = n3.lastname);

-- Test of empty string (pre-existing in `names` table) mixed with NULL (generated from outer join)
select n1.id,n2.id from names n1 left join names n2 on (n1.firstname = 'abcd');
select n1.id,n2.id,n3.id from names n1 left join names n2 on (n1.firstname = 'abcd') left join names n3 on (n2.firstname = n3.lastname);
select n1.id,n2.lastname from names n1 left join names n2 on (n1.firstname = 'abcd') union select id,lastname from names where lastname = '';
select * from (select n1.id,n2.lastname from names n1 left join names n2 on (n1.firstname = 'abcd') union select id,lastname from names n3 where n3.lastname = '' or n3.id < 3) n4 order by lastname,id;
select * from (select n1.id,n2.lastname from names n1 left join names n2 on (n1.firstname = 'abcd') union select id,lastname from names n3 where n3.lastname = '' or n3.id < 3) n4 where n4.lastname is NULL order by lastname,id;
select * from (select n1.id,n2.lastname from names n1 left join names n2 on (n1.firstname = 'abcd') union select id,lastname from names n3 where n3.lastname = '' or n3.id < 3) n4 where n4.lastname is NOT NULL order by lastname,id;
select * from (select n1.id,n2.lastname from names n1 left join names n2 on (n1.firstname = 'abcd') union select id,lastname from names n3 where n3.lastname = '' or n3.id < 3) n4 where n4.lastname = '' order by lastname,id;

