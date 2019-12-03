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
       (a+b+c+d+e)/5,
       CASE WHEN a+1=b THEN 111 WHEN a+1=c THEN 222
        WHEN a+1=d THEN 333  WHEN a+1=e THEN 444 ELSE 555 END,
       abs(b-c),
       b-c
  FROM t1
 WHERE a>b
 ORDER BY 2,5,3,4,1
;
