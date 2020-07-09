
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
-- TCEF03 : OCTO553 : type coercions should be consistent
-- type mismatches are errors
SELECT GREATEST(1, '2');
SELECT GREATEST(10, '2');
SELECT GREATEST('10', 2);
SELECT GREATEST(1, NULL, '2');
SELECT GREATEST(1, NULL, '2', 2.4);

SELECT LEAST(1, '2');
SELECT LEAST(10, '2');
SELECT LEAST('10', 2);
SELECT LEAST(1, NULL, '2');
SELECT LEAST(1, NULL, '2', 2.4);
