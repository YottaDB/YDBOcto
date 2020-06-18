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

-- TCJ09 : OCTO529 :  Query with lots of CROSS JOIN runs for ever if WHERE clause contains lots of AND and = operators
-- This is tested extensively by the `test_sqllogic/select5` subtest. So no queries are added for this specifically.

-- The below query is not directly related to OCTO529 in that it does not take for ever to run with or without the OCTO529 fixes
-- But with an interim state of the OCTO529 fixes where CROSS JOINs were freely reordered, this query exposed an issue where
-- a CROSS JOIN cannot be moved across an OUTER JOIN. The below two queries (which differ only in the placement of the CROSS JOIN
-- give different outputs and therefore are not equivalent).
SELECT DISTINCT names.firstName, COUNT(alias1.firstName), alias1.id FROM names  RIGHT JOIN names AS alias1 ON ((names.firstName = alias1.firstName) AND NOT (names.id <= alias1.id)) CROSS JOIN names AS alias2 GROUP BY alias1.id, names.firstName;
SELECT DISTINCT names.firstName, COUNT(alias1.firstName), alias1.id FROM names  CROSS JOIN names as alias2 RIGHT JOIN names AS alias1 ON ((names.firstName = alias1.firstName) AND NOT (names.id <= alias1.id)) GROUP BY alias1.id, names.firstName;

