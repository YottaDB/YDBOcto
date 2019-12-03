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

SELECT a,
       d-e,
       c,
       a+b*2,
       e
  FROM t1
 WHERE a>b
    OR c>d
    OR (c<=d-2 OR c>=d+2)
 ORDER BY 2,3,5,4,1
;
