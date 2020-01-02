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

-- TGB04 : OCTO55 : Test octo -vv and logical plan output for sample GROUP BY query

SELECT DISTINCT 1 + COUNT(n1.id * 2),n1.firstname from names n1 where n1.id IN (SELECT DISTINCT MAX((n2.id % 3) + 4) from names n2 GROUP BY n2.firstname) GROUP BY n1.firstname ORDER BY 2, 1;

-- OCTO412 : Test that we do not generate M code to unnecessarily compute duplicate aggregate function usage.
-- In the below query, currently COUNT(id) in the SELECT column list and ORDER BY 2 are treated as 1 aggregate function
--	and COUNT(id) in the ORDER BY column list is treated as a 2nd aggregate function use so we expect to see
--	2 aggregate function counts generated in the physical plan. When #414 is fixed, we will see only 1 aggregate function count.
SELECT firstname,COUNT(id) FROM names GROUP BY firstname ORDER BY 2,COUNT(id);

