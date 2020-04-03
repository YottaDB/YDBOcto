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

-- TGB10 : OCTO453 : SELECT DISTINCT returns incorrect results when used with GROUP BY and OR operator in WHERE clause

-- Below query is the one that failed before OCTO453 was fixed
SELECT DISTINCT firstname,COUNT(firstname) FROM names WHERE firstname = 'Zero' OR firstname = 'Zero' GROUP BY firstname;

-- Below are queries which did not fail but are related to the above and so included here for regression testing.

SELECT DISTINCT firstname,COUNT(firstname) FROM names WHERE firstname = 'Zero' GROUP BY firstname;

SELECT firstname,COUNT(firstname) FROM names WHERE firstname = 'Zero' GROUP BY firstname;
SELECT firstname,COUNT(firstname) FROM names WHERE firstname = 'Zero' OR firstname = 'Zero' GROUP BY firstname;

SELECT DISTINCT firstname FROM names WHERE firstname = 'Zero';
SELECT DISTINCT firstname FROM names WHERE firstname = 'Zero' OR firstname = 'Zero';

SELECT firstname FROM names WHERE firstname = 'Zero';
SELECT firstname FROM names WHERE firstname = 'Zero' OR firstname = 'Zero';

