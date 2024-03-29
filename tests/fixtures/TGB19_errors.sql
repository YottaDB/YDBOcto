#################################################################
#								#
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TGB19 : OCTO767 : Issue ERR_GROUP_BY_OR_AGGREGATE_FUNCTION error even if GROUP BY in sub query uses outer query columns

SELECT id,firstname FROM names n1 WHERE id IN (SELECT n2.id FROM names n2 group by n1.id);
SELECT id,firstname FROM names n1 GROUP BY id HAVING 1 IN (SELECT n2.id FROM names n2 group by n1.id);
