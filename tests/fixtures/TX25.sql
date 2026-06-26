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

-- TX25 : YDBAIM#96 : a column with a transformation function.

-- When a READONLY column needs a conversion before it can be indexed - a BOOLEAN through
-- $$ForceBoolean, a date through $$Transform2UnixTime, a piece-of-piece through
-- $$PieceOfPieceXform - Octo asks AIM to store the CONVERTED value. That is AIM type=2, and
-- composed with a record specification it becomes type=6.

-- This test is TX22's structure reduced to one column, so that the transformation path is
-- exercised rather than re-tested:

--   * "active" -> ^EVT(keys("id"),"ACT") PIECE 1, declared BOOLEAN

-- Rows:

--   * id 1 : ^EVT(1,"ACT")="1"     -> active is true
--   * id 2 : ^EVT(2,0)="Bob"       -> no ACT node; active is a missing-node NULL
--   * id 3 : ^EVT(3)="Carol"       -> a value on the row node and no children at all; active is
--                                     also a missing-node NULL, but there is no node at the
--                                     indexed depth for type=3 to notice

-- WHERE active IS NULL must return ids 2 and 3, the same as a full table scan: the NULL bucket
-- holds the transformation applied to the empty string, so IS NULL keeps working.
CREATE TABLE events (
 id INTEGER PRIMARY KEY,
 name VARCHAR(20) GLOBAL "^EVT(keys(""id""),0)" PIECE 1,
 active BOOLEAN GLOBAL "^EVT(keys(""id""),""ACT"")" PIECE 1
) GLOBAL "^EVT" AIMTYPE 1 DELIM "^" READONLY;

-- Unfiltered: the row universe from a full table scan, all three ids.
SELECT * FROM events ORDER BY id;

-- WHERE on the transformed column. Correct answer is ids 2 and 3.
SELECT * FROM events WHERE active IS NULL ORDER BY id;
