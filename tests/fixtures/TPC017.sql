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

-- TPC017 : OCTO502 : AS keyword supports list of column name aliases (on top of table name alias

SELECT * FROM names AS abcd(a1,b1,c1) LIMIT 1;
SELECT * FROM (SELECT * FROM names) AS abcd(a2,b2,c2) LIMIT 1;
SELECT a3,b3,c3 FROM (SELECT * FROM names) AS abcd(a3,b3,c3) LIMIT 1;
SELECT * FROM names AS abcd(a4,b4) LIMIT 1;
selECT * FROM (SELECT * FROM names) AS abcd(a5,b5) LIMIT 1;
SELECT a6 FROM (VALUES (1)) as MyTable(a6);
SELECT * FROM (VALUES (1), (3), (5), (7), (9) ) AS MyTable(a7);
SELECT * FROM (VALUES (1, 2), (3, 4), (5, 6), (7, 8), (9, 10) ) AS MyTable(a8,b8);
SELECT b9,a9 FROM (VALUES (1, 2), (3, 4), (5, 6), (7, 8), (9, 10) ) MyTable(a9, b9);
SELECT * FROM names WHERE (id = (SELECT x FROM (VALUES(id)) AS tbl(x)));

