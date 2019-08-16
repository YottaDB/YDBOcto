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
# Derived from https://github.com/shivarajugowda/jdbcSQLTest

SELECT d-e,
       abs(b-c),
       (a+b+c+d+e)/5
  FROM t1
 WHERE (c<=d-2 OR c>=d+2)
   AND (e>a AND e<b)
 /*ORDER BY 2,3,1*/
;