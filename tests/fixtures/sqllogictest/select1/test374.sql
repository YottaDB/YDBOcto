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

SELECT b,
       b-c,
       a+b*2+c*3,
       abs(a),
       c-d,
       a,
       d-e
  FROM t1
 WHERE b>c
 ORDER BY 4,6,1,3,7,2,5
;
