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

SELECT CASE a+1 WHEN b THEN 111 WHEN c THEN 222
        WHEN d THEN 333  WHEN e THEN 444 ELSE 555 END,
       b-c,
       e,
       (SELECT count(*) FROM t1 AS x WHERE x.b<t1.b),
       c,
       a,
       a+b*2
  FROM t1
 WHERE (e>a AND e<b)
   AND (e>c OR e<d)
 ORDER BY 3,6,2,1,5,7,4
;
