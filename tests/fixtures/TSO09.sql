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

-- TSO09 : UNION of SELECT and another SET operation

SELECT * FROM names n1 INNER JOIN names n2 ON n1.id = n2.id
UNION ALL
(
  SELECT id, firstName, lastName, NULL, NULL, '' FROM names
  EXCEPT ALL
  SELECT n1.id, n1.firstName, n1.lastName, ''::text, NULL, NULL FROM names n1 INNER JOIN names n2 ON n1.id = n2.id
);

