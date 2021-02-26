#################################################################
#								#
# Copyright (c) 2020-2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
SELECT GREATEST(0, 1, 2);
SELECT GREATEST(2, 0, 1);
SELECT GREATEST(0, 0, 0);
SELECT GREATEST('a', 'b', 'c');
SELECT GREATEST('1', '2', '3');
SELECT GREATEST('10', '2', '3'); -- should give '3'
SELECT GREATEST(10, 2, 3); -- should give 10
-- NULL is ignored unless all arguments are NULL
SELECT GREATEST(NULL); -- should be NULL
SELECT LEAST(NULL, NULL, NULL); -- should be NULL
SELECT GREATEST(NULL, 1); -- should be 1
SELECT GREATEST(NULL, 1, NULL); -- should still be 1

SELECT LEAST(0, 1, 2);
SELECT LEAST(2, 0, 1);
SELECT LEAST(0, 0, 0);
SELECT LEAST('a', 'b', 'c');
SELECT LEAST('1', '2', '3');
SELECT LEAST('10', '2', '3'); -- should give '10'
SELECT LEAST(10, 2, 3); -- should give 2
SELECT LEAST(NULL); -- should be NULL
SELECT LEAST(NULL, NULL, NULL); -- should be NULL
SELECT LEAST(NULL, 1); -- 1
SELECT LEAST(NULL, 1, NULL); -- 1
