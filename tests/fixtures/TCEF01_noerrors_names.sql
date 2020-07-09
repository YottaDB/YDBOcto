#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- All queries in this file are valid.
-- TCEF01 : OCTO539 : COALESCE

-- SELECT literals
SELECT COALESCE(NULL);
SELECT COALESCE(1);
SELECT COALESCE(NULL, 1, 2);
SELECT COALESCE(NULL, 1, NULL);

-- literals which require type promotion
SELECT COALESCE(1, 1.2);
SELECT COALESCE(1.2, 1);
SELECT COALESCE(1, 2, 3, 1.2);

-- SELECT tables
SELECT COALESCE(1) FROM names; -- should output 1 for each row
SELECT COALESCE(1) FROM names WHERE 1 = 0; -- should have no results

-- SELECT subqueries
SELECT COALESCE(
	(SELECT firstName FROM names WHERE 1 = 0),
	(SELECT NULL::VARCHAR)
);
SELECT COALESCE(
	(SELECT firstName::VARCHAR FROM names WHERE lastName = 'Cool' LIMIT 1),
	(SELECT NULL::VARCHAR),
	(SELECT 'a'::VARCHAR)
);

-- TODO: once #320 is implemented, add tests for SELECTs from columns that are NULL
