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

-- TSO07 : Simulate outer join using UNION

SELECT a.id as A_id, b.id as B_id FROM names a INNER JOIN names b ON a.firstName = b.firstName
UNION
SELECT id as A_id, NULL AS B_id FROM names a WHERE a.firstName NOT IN (SELECT b.firstName FROM names b);

