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

-- TSO08 : UNION of tables with different numbers of keys

SELECT t1.id, t1.firstName, t1.lastName FROM names t1 INNER JOIN (SELECT * FROM names) t2 ON t1.id = t2.id
UNION ALL
SELECT * FROM names t1;

