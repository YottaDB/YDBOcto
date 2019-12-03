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

SELECT c,
       c-d,
       a+b*2+c*3+d*4,
       a+b*2+c*3+d*4+e*5,
       abs(b-c),
       (a+b+c+d+e)/5,
       (SELECT count(*) FROM t1 AS x WHERE x.c>t1.c AND x.d<t1.d)
  FROM t1
 WHERE d NOT BETWEEN 110 AND 150
   AND a>b
   AND c>d
 ORDER BY 7,1,5,3,2,4,6
;
