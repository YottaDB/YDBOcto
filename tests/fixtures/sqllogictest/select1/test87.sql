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
       d,
       a+b*2+c*3+d*4+e*5,
       a-b,
       (SELECT count(*) FROM t1 AS x WHERE x.c>t1.c AND x.d<t1.d)
  FROM t1
 WHERE c BETWEEN b-2 AND d+2
    OR (a>b-2 AND a<b+2)
    OR b>c
 /*ORDER BY 1,3,5,4,2*/
;