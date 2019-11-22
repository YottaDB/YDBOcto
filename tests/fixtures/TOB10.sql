#################################################################
#								#
# Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TOB10 : OCTO384 : ORDER BY using column name derived from a sub-query returns incorrect results

-- Column name derived from 1-level-deep sub-query
SELECT * FROM (SELECT id, id%2 AS newid FROM names) n1 ORDER BY newid;

-- Column name derived from 2-level-deep sub-query
SELECT * FROM (SELECT * FROM (SELECT id, id%2 AS newid FROM names) n1) n2 ORDER BY newid;
SELECT * FROM (SELECT id,firstname,newid FROM (SELECT id, id%2 AS newid, firstname FROM names) n1) n2 ORDER BY newid;

-- Column name derived from 2-level-deep sub-query with SET operations
SELECT * FROM (SELECT id,newid1 FROM (SELECT id, id%2 AS newid1, firstname FROM names UNION ALL SELECT id, id%3 AS newid2, lastname FROM names) n1) n2 ORDER BY newid1;
SELECT id,newid1 FROM (SELECT * FROM (SELECT id, id%2 AS newid1, firstname FROM names UNION ALL SELECT id, id%3 AS newid2, lastname FROM names) n1) n2 ORDER BY newid1;

-- ORDER BY using multiple columns from sub-query
SELECT * FROM (SELECT id, id%2 AS newid2, id%3 AS newid3 FROM names) n1 ORDER BY newid3, newid2 desc;
SELECT * FROM (SELECT * FROM (SELECT id, id%2 AS newid2, id%3 AS newid3 FROM names) n1) n2 ORDER BY newid3 desc, newid2;

-- ORDER BY column name derived from sub-query but is not the first column name encountered in the SET/UNION operation. Should error out
SELECT * FROM (SELECT * FROM (SELECT id, id%2 AS newid1, firstname FROM names UNION ALL SELECT id, id%3 AS newid2, lastname FROM names) n1) n2 ORDER BY newid2;

-- Repeat same set of queries above but using MODULO function instead of % operator
SELECT * FROM (SELECT id, MODULO(id,2) AS newid FROM names) n1 ORDER BY newid;
SELECT * FROM (SELECT * FROM (SELECT id, MODULO(id,2) AS newid FROM names) n1) n2 ORDER BY newid;
SELECT * FROM (SELECT id,firstname,newid FROM (SELECT id, MODULO(id,2) AS newid, firstname FROM names) n1) n2 ORDER BY newid;
SELECT * FROM (SELECT id,newid1 FROM (SELECT id, MODULO(id,2) AS newid1, firstname FROM names UNION ALL SELECT id, MODULO(id,3) AS newid2, lastname FROM names) n1) n2 ORDER BY newid1;
SELECT id,newid1 FROM (SELECT * FROM (SELECT id, MODULO(id,2) AS newid1, firstname FROM names UNION ALL SELECT id, MODULO(id,3) AS newid2, lastname FROM names) n1) n2 ORDER BY newid1;
SELECT * FROM (SELECT id, MODULO(id,2) AS newid2, MODULO(id,3) AS newid3 FROM names) n1 ORDER BY newid3, newid2 desc;
SELECT * FROM (SELECT * FROM (SELECT id, MODULO(id,2) AS newid2, MODULO(id,3) AS newid3 FROM names) n1) n2 ORDER BY newid3 desc, newid2;
SELECT * FROM (SELECT * FROM (SELECT id, MODULO(id,2) AS newid1, firstname FROM names UNION ALL SELECT id, MODULO(id,3) AS newid2, lastname FROM names) n1) n2 ORDER BY newid2;

