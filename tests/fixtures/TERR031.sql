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

-- TERR031 : OCTO502 : Error scenarios in AS column name alias list

-- Test of ERR_AS_MORE_COLUMNS
SELECT * FROM names AS abcd(a,b,c,d);
SELECT a,b,c FROM (SELECT firstname,lastname FROM names) AS abcd(a,b,c,d);
SELECT col1 FROM (SELECT 1 AS col1) AS tbl(a,b);

-- Test of ERR_UNKNOWN_COLUMN_NAME
SELECT id,firstname,lastname FROM names AS abcd(a,b,c);
SELECT a,lastname,c FROM (SELECT * FROM names) AS abcd(a,b,c);

-- Test of ERR_MISSING_FROM_ENTRY
SELECT names.id,b,c FROM names AS abcd(a,b,c);

-- Test of ERR_AS_MORE_COLUMNS with VALUES clause
SELECT * FROM (VALUES (1), (3), (5), (7), (9) ) AS MyTable(a,b);

-- Test of ERR_UNKNOWN_COLUMN_NAME with VALUES clause
SELECT * FROM names WHERE (id = (SELECT x FROM (VALUES(y)) AS tbl(x)));

