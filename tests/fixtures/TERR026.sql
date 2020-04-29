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

-- TERR026 : OCTO345 : Issue error for if number of function arguments exceeds M limit

-- Too many arguments in function declaration
CREATE FUNCTION TOOMANYARGS(INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER) RETURNS VARCHAR AS $$TOOMANYARGS;

-- Too many arguments in function call
SELECT * FROM names where id = ABS(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33);

