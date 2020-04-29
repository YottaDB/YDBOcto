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

-- TCF016 : OCTO345 : CREATE FUNCTION with alternative type names

CREATE FUNCTION ABS(INT) RETURNS DEC AS $$ABS^%ydboctosqlfunctions;
SELECT ABS(-1) FROM names;

CREATE FUNCTION ABS(SMALLINT) RETURNS DECIMAL AS $$ABS^%ydboctosqlfunctions;
SELECT ABS(-2) FROM names;
