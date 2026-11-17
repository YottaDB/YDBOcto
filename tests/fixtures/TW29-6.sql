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

-- TW29 Case 6 (YDBOcto#1191) : the column's GLOBAL has no key subscript. "val" is at ^GB, one node that holds the val of
-- every row. AIM has no subscript of that node to cross reference, so Octo reads every row instead. Before the fix,
-- every query below but the first two and the MIN/MAX on "barekeys" failed with %YDBAIM-F-NOSUBS.
CREATE TABLE barekeys (
 a INTEGER,
 b INTEGER,
 val VARCHAR(20) GLOBAL "^GB" PIECE 1,
 PRIMARY KEY (a, b)
) GLOBAL "^GROWC" READONLY;
-- The three rows of the table
SELECT * FROM barekeys ORDER BY a, b;
-- WHERE on the column: every row has val = 'hit', so all three rows
SELECT * FROM barekeys WHERE val = 'hit' ORDER BY a, b;
-- ORDER BY the column: the first two rows
SELECT * FROM barekeys ORDER BY val, a, b LIMIT 2;
-- MIN/MAX of the column: hit and hit (a table with several key columns was never optimized, so this worked before too)
SELECT MIN(val), MAX(val) FROM barekeys;

-- The same column on a table with one key column, whose MIN/MAX is optimized
CREATE TABLE barekeys1 (
 a INTEGER PRIMARY KEY,
 val VARCHAR(20) GLOBAL "^GB" PIECE 1
) GLOBAL "^GROWD" READONLY;
-- The two rows of the table
SELECT * FROM barekeys1;
-- MIN/MAX of the column: hit and hit
SELECT MIN(val), MAX(val) FROM barekeys1;
