#################################################################
#								#
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TWI14 : OCTO959 : Verify IN operator in sub-query used with WHERE clause and OR operator in outer query

SELECT (SELECT firstname FROM names WHERE lastname IN ('Nikon','2')) FROM names WHERE (firstname = 'Lord') OR (firstname = 'Zero');

