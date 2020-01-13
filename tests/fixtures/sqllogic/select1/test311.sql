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

SELECT abs(b-c),
       (a+b+c+d+e)/5,
       d,
       b,
       (SELECT count(*) FROM t1 AS x WHERE x.c>t1.c AND x.d<t1.d),
       c
  FROM t1
 ORDER BY 4,6,2,1,5,3
;