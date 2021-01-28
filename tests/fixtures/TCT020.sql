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

-- TCT020 : OCTO676 : Casting NULL values to INTEGER/NUMERIC/BOOLEAN type returns incorrect results in some cases

SELECT (SELECT (SELECT NULL::INTEGER) FROM names ORDER BY 1 LIMIT 1)::NUMERIC is NULL;
SELECT (SELECT (SELECT NULL::INTEGER) FROM names ORDER BY 1 LIMIT 1)::INTEGER is NULL;
SELECT (SELECT (SELECT NULL::INTEGER) FROM names ORDER BY 1 LIMIT 1)::BOOLEAN is NULL;
SELECT +(SELECT (SELECT NULL::INTEGER) FROM names ORDER BY 1 LIMIT 1) is NULL;
SELECT -(SELECT (SELECT NULL::INTEGER) FROM names ORDER BY 1 LIMIT 1) is NULL;

SELECT (SELECT (SELECT NULL::NUMERIC) FROM names ORDER BY 1 LIMIT 1)::NUMERIC is NULL;
SELECT (SELECT (SELECT NULL::NUMERIC) FROM names ORDER BY 1 LIMIT 1)::INTEGER is NULL;
-- The below query issues an error in Postgres but not in Octo and hence is commented out.
-- SELECT (SELECT (SELECT NULL::NUMERIC) FROM names ORDER BY 1 LIMIT 1)::BOOLEAN is NULL;
SELECT +(SELECT (SELECT NULL::NUMERIC) FROM names ORDER BY 1 LIMIT 1) is NULL;
SELECT -(SELECT (SELECT NULL::NUMERIC) FROM names ORDER BY 1 LIMIT 1) is NULL;

-- The below query issues an error in Postgres but not in Octo and hence is commented out.
-- SELECT (SELECT (SELECT NULL::BOOLEAN) FROM names ORDER BY 1 LIMIT 1)::NUMERIC is NULL;
SELECT (SELECT (SELECT NULL::BOOLEAN) FROM names ORDER BY 1 LIMIT 1)::INTEGER is NULL;
SELECT (SELECT (SELECT NULL::BOOLEAN) FROM names ORDER BY 1 LIMIT 1)::BOOLEAN is NULL;
-- The below query issues an error in both Postgres and Octo and hence is commented out.
-- SELECT +(SELECT (SELECT NULL::BOOLEAN) FROM names ORDER BY 1 LIMIT 1) is NULL;
-- The below query issues an error in both Postgres and Octo and hence is commented out.
-- SELECT -(SELECT (SELECT NULL::BOOLEAN) FROM names ORDER BY 1 LIMIT 1) is NULL;

