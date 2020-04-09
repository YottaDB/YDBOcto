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

-- TGB12 : OCTO479 : Hang when GROUP BY in sub-query uses all columns from parent query (and no columns from sub-query)

SELECT * FROM names n1 WHERE n1.id = (SELECT 1 FROM names n2 GROUP BY n1.id,n1.firstname LIMIT 1);

