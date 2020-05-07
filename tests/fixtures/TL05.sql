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

-- TL05 : Simple LIMIT tests

-- select with limit of one row
select * from names limit 1;

-- select with limit of one row from nested query
select * from (select * from names limit 1);

-- select with limit of one on outer statement of join
select * from names n1 INNER JOIN names n2 ON (n1.id = n2.id) WHERE n1.firstName = 'Joey' LIMIT 1;
select * from names n1 INNER JOIN names n2 ON (n1.id = n2.id) WHERE n1.firstName > 'Cereal' LIMIT 1;

-- -- error cases
select * from names limit .;
select * from names limit;
select * from names limit 'abcd';
select * from names limit -3;
