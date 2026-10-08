#################################################################
#								#
# Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TJC025 : OCTO1158 : LIKE / ILIKE with a bind parameter as the pattern ("WHERE v LIKE ?" from a JDBC prepared statement)
-- Rocto plans the query when it gets the Parse message, before the value is known. The best plan depends on the value:
-- 'PAT%' can walk the cross reference of column v over the prefix 'PAT', '%PAT' cannot. So rocto picks the plan again
-- when each Bind message supplies the value. This test verifies that:
--   1. "v LIKE ?" returns the same rows as "COALESCE(v, NULL) LIKE ?" (not on a column, so it checks every row).
--   2. Each kind of bound value gets the expected plan.
--   3. A table only queried with values that have no prefix gets no cross reference.

CREATE TABLE TJC025 (id INTEGER PRIMARY KEY, v VARCHAR) GLOBAL "^TJC025";
INSERT INTO TJC025 VALUES (1,'PAT'),(2,'PATIENT'),(3,'PATENT'),(4,'PAU'),(5,'PA'),(6,'12'),(7,'120'),(8,'12a'),(9,'2');
INSERT INTO TJC025 VALUES (10,'PAT%X'),(11,'PAT_Y'),(12,NULL),(13,''),(14,'añb'),(15,'añ'),(16,'ao'),(17,'pat');
SELECT * FROM TJC025 ORDER BY id;
-- Same data in a second table, which TJC025.java only queries with patterns that have no prefix
CREATE TABLE TJC025B (id INTEGER PRIMARY KEY, v VARCHAR) GLOBAL "^TJC025B";
INSERT INTO TJC025B SELECT * FROM TJC025;
