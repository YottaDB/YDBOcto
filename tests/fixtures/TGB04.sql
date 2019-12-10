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

-- TGB04 : OCTO55 : Test octo -vv and logical plan output for sample GROUP BY query

SELECT DISTINCT 1 + COUNT(n1.id * 2),n1.firstname from names n1 where n1.id IN (SELECT DISTINCT MAX((n2.id % 3) + 4) from names n2 GROUP BY n2.firstname) GROUP BY n1.firstname ORDER BY 2, 1;

