#################################################################
#								#
# Copyright (c) 2021-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TCJ13 : OCTO769 : Speed up CROSS JOIN by rearranging FROM/JOIN even if OR is used in WHERE clause

-- This is a query in tests/fixtures/sqllogic/select4.test (which gets run as part of the test_sqllogic/TS04 bats subtest
-- if this query gets randomly chosen from the many queries in that file) that used to take 15 minutes to run.
-- After the YDBOcto#769 fixes to rearrange CROSS JOIN independently for each DNF plan, it takes less than a second to run.

SELECT e6, e8+376, x4, x9, e5*680, d2+210, d3
  FROM s4t3, s4t6, s4t8, s4t2, s4t5, s4t9, s4t4
 WHERE (18=c5 OR c5=31 OR 915=c5)
   AND (b4=810 OR b4=849 OR 653=b4 OR b4=288)
   AND a2 in (543,324,253)
   AND a3 in (651,964,197,234,995,364,478)
   AND c9 in (126,992,732,811,327,800,87)
   AND e8=a5
   AND d6=d9
;

