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

-- TCJ09 : OCTO529 :  Query with lots of CROSS JOIN runs forever if WHERE clause contains lots of AND and = operators
-- This is tested extensively by the `test_sqllogic/select5` subtest. So no queries are added for this specifically.

-- TCJ11 : OCTO529 : Test query with only ONE CROSS JOIN is also reordered/optimized
select * from names n1 cross join names n2 where n2.id = 3 and n1.id = n2.id;

