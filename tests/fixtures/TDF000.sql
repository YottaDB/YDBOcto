#################################################################
#								#
# Copyright (c) 2020-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TDF000 : OCTO345 : DROP FUNCTION doesn't work with octo-seed functions (ABS)

SELECT ABS(-1) FROM names;
DROP FUNCTION ABS (INTEGER);
SELECT ABS(-1) FROM names;

SELECT ABS(-1.0) FROM names;
DROP FUNCTION ABS (NUMERIC);
SELECT ABS(-1.0) FROM names;

