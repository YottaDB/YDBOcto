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

-- Test that column aliases are inherited from subquery into parent query column alias
SELECT id,(SELECT firstname FROM names LIMIT 1), lastname FROM names;
SELECT id,(SELECT firstname FROM names LIMIT 1 UNION SELECT firstname FROM names LIMIT 1), lastname FROM names;

-- Test that queries that only differ in alias names do show the respective alias names in the column header
SELECT id AS ALIAS1 FROM names;
SELECT id AS ALIAS2 FROM names;

