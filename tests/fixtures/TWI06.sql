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

-- TWI06 : WHERE in sub-query only references TWO parent query columns that match (variant of T0006)

SELECT *
FROM names n1
INNER JOIN names n2
ON (n1.id = n2.id)
WHERE n1.firstName IN (
  SELECT n3.firstName FROM names n3
  WHERE n1.id = n2.id
)

