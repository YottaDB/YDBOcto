#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TCJ14 : OCTO769 : Verify CROSS JOIN reordering optimization happens INDEPENDENTLY in each plan if OR is used in WHERE clause

-- This is a query in tests/fixtures/TX04.sql (gets run as part of the test_xref/TX04 bats subtest) where the CROSS JOIN
-- optimization happens INDEPENDENTLY for each of the 2 plans that get generated (one for each OR operand).
-- This query is duplicated here just to do the INDEPENDENTLY reordered verification.

SELECT * FROM names n1, names n2                   WHERE (n1.lastname IS NULL) OR  (n2.lastname IS NULL);

