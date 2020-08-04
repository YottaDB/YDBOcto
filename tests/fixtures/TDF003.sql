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

-- TDF003 : OCTO562 : DROP FUNCTION works with parameterless functions

-- Test DROP FUNCTION with parentheses for paramater type list
CREATE FUNCTION PARMLESSFUNC() RETURNS VARCHAR AS $$^PARMLESSFUNC;
SELECT PARMLESSFUNC() FROM names;
DROP FUNCTION PARMLESSFUNC ();
SELECT PARMLESSFUNC() FROM names;

-- Test DROP FUNCTION without parentheses for paramater type list
CREATE FUNCTION PARMLESSFUNC() RETURNS VARCHAR AS $$^PARMLESSFUNC;
SELECT PARMLESSFUNC() FROM names;
DROP FUNCTION PARMLESSFUNC;
SELECT PARMLESSFUNC() FROM names;

