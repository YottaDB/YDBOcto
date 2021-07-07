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

-- TDF006 : OCTO595 : DROP FUNCTION IF EXISTS issues a warning when the specified function doesn't exist

CREATE FUNCTION PARMLESSFUNC() RETURNS VARCHAR AS $$^PARMLESSFUNC;
SELECT PARMLESSFUNC() FROM names LIMIT 1;
DROP FUNCTION IF EXISTS PARMLESSFUNC ();
SELECT PARMLESSFUNC() FROM names LIMIT 1;
DROP FUNCTION IF EXISTS PARMLESSFUNC ();

CREATE FUNCTION PARMFULLFUNC(NUMERIC) RETURNS NUMERIC AS $$samevalue^functions;
SELECT PARMFULLFUNC(3.14) FROM names LIMIT 1;
DROP FUNCTION IF EXISTS PARMFULLFUNC (NUMERIC);
SELECT PARMFULLFUNC(3.14) FROM names LIMIT 1;
DROP FUNCTION IF EXISTS PARMFULLFUNC (NUMERIC);

