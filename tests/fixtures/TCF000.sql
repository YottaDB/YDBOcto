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

-- TCF000 : OCTO345 : CREATE FUNCTION for function that already exists, i.e. SQL `ABS` function.
-- Note: the OID (last column of pg_proc) should be incremented between the two CREATE FUNCTION calls below.

CREATE FUNCTION ABS(INTEGER) RETURNS NUMERIC AS $$ABS^%ydboctosqlfunctions;
SELECT proname,prorettype,proargtypes FROM pg_catalog.pg_proc where proname = 'ABS';
SELECT 'OID',oid FROM pg_catalog.pg_proc where proname = 'ABS';
SELECT ABS(-1) FROM names;

CREATE FUNCTION ABS(NUMERIC) RETURNS NUMERIC AS $$ABS^%ydboctosqlfunctions;
SELECT proname,prorettype,proargtypes FROM pg_catalog.pg_proc where proname = 'ABS';
SELECT 'OID',oid FROM pg_catalog.pg_proc where proname = 'ABS';
SELECT ABS(-1) FROM names;
