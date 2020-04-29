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

-- TCF003 : OCTO345 : Report syntax error when incorrect number of parameters passed to function

-- Test detection of single missing function argument
select ABS() as missingarg from names;

-- Test detection of multiple missing function arguments
select REPLACE('balloons') as missingargs from names;

-- Test detection of single extra function argument on multi-parameter function
select PG_CATALOG.PG_GET_EXPR('balloons', 11, 'bananas') as extraarg from names;

-- Test detection of multiple extra function argument on single parameter function
select ABS(12, 'lots', 'of', 'extra', 'args') as extraargs from names;

-- Test detection of single extra function argument on parameterless function
select VERSION('should be no args here') as extraarg from names;

-- Test detection of multiple extra function arguments on parameterless function
select VERSION('should', 'be', 'no', 'args', 'here') as extraargs from names;
