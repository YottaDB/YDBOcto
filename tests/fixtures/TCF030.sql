#################################################################
#								#
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TCF030 : OCTO816 : SQL functions accept NULL values without additional CREATE FUNCTION definitions

SELECT CONCAT(NULL, NULL);
SELECT CONCAT('myarg', NULL);
SELECT CONCAT(NULL, 'myarg');
SELECT CONCAT(NULL, NULL, NULL);
SELECT CONCAT(NULL, 'abcd', NULL);
SELECT CONCAT('abcd', NULL, NULL);
SELECT CONCAT(NULL, NULL, 'abcd');
SELECT CONCAT(NULL, 'abcd', 'abcd');
SELECT CONCAT('abcd', 'abcd', NULL);
SELECT CONCAT('abcd', NULL, 'abcd');
SELECT ABS(NULL);
SELECT ABS((select id from names where firstname = 'noname'));
