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

-- TWI07 : WHERE in sub-query only references ONE parent query column AND a CONSTANT (variant of T0013)

SELECT *
FROM names n1
WHERE n1.firstName IN (SELECT n2.firstName
  FROM names n2
  WHERE n1.firstName = "Acid"
);

