#################################################################
#								#
# Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TERR038 : OCTO600 : Test syntax highlighting omitted when whole excerpt or query is highlighted

-- Too many arguments in function declaration
CREATE FUNCTION TOOMANYARGS(INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER) RETURNS VARCHAR AS $$TOOMANYARGS;
SELECT * FROM names where firstname = TOOMANYARGS(1, '2', 3, '2', 5, '2', 7, '2', 9, '2', 11, '2', 13, '2', 15, '2', 17, '2', 19, '2', 21, '2', 23, '2', 25, '2', 27, '2', 29, '2', 31, '2', 33);

-- Too many arguments in function call
SELECT * FROM names where id = ABS(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33);

-- Too many arguments in function declaration (multiline)
CREATE FUNCTION TOOMANYARGS(INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR,
	INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR,
	INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR, INTEGER, VARCHAR,
	INTEGER, VARCHAR, INTEGER) RETURNS VARCHAR AS $$TOOMANYARGS;
SELECT * FROM names where firstname = TOOMANYARGS(1, '2', 3, '2', 5, '2', 7, '2', 9, '2', 11, '2', 13, '2', 15, '2',
	17, '2', 19, '2', 21, '2', 23, '2', 25, '2', 27, '2', 29, '2', 31, '2', 33);

-- Too many arguments in function call (multiline)
SELECT * FROM names where id = ABS(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
	17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33);
