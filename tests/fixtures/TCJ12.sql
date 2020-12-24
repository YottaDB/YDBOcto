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

-- TCJ12 : OCTO652 : Incorrect results if CROSS JOIN is used with OR operator in the WHERE clause

SELECT * FROM names n1, names n2 WHERE n1.id = 1 OR n2.id = 2;
SELECT * FROM names n1, names n2, names n3 WHERE n1.id = 1 OR n2.id = 2 OR n3.id = 3;

