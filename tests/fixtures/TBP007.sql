#################################################################
#								#
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TBP007 : OCTO519 : Test qualified identifers accepted for SELECT, INSERT INTO, UPDATE, DELETE FROM, and TRUNCATE

-- Syntax should be accepted, but table or column may not exist
SELECT * FROM names n1, names n2 where N1.FIRSTNAME = 2;
INSERT INTO test.names SELECT * from names;
DELETE FROM test.names where lastname = 'Cool';
UPDATE test.names SET id = 3, name = "Sunny" WHERE temperature = "warm";
TRUNCATE test.names;
