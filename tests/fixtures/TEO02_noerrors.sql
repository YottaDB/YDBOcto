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

-- All queries in this query file are valid queries that do not issue any error.

-- TEO02 : OCTO496 : EXISTS on a sub query that has UNION operations fails assertions

-- Below queries used to fail asserts before the YDBOcto#496 code fix
SELECT EXISTS (SELECT 1 UNION SELECT 2);
SELECT EXISTS (SELECT 1,2 UNION SELECT 3,4);
SELECT EXISTS (SELECT * FROM (SELECT * FROM names UNION SELECT * FROM names) n1);

-- Below query used to return incorrect results before the YDBOcto#496 code fix
SELECT * FROM (SELECT * FROM names UNION SELECT * FROM names) n1 WHERE firstname = 'Zero';

