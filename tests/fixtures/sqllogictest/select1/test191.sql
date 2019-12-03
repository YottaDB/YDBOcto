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

SELECT abs(a),
       abs(b-c),
       c,
       a-b,
       c-d,
       a+b*2+c*3+d*4,
       b
  FROM t1
 ORDER BY 2,6,7,4,1,5,3
;
