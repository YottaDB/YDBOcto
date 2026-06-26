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

-- TX22 : YDBAIM#96 : columns stored at more than one depth below the row.

-- The primary key is (id), so a row is ^USER(id). This table stores its columns at two different
-- depths beneath that row:

--   * "name"  -> ^USER(id,0)          : one level down, the FileMan-style "0" node.
--   * "email" -> ^USER(id,"EMAIL")    : one level down, keyed by a fixed subscript. This is the
--                                       shape the AIMTYPE documentation uses for its example.
--   * "dept"  -> ^USER(id,1,"DEPT")   : TWO levels down. Nothing in the documentation covers this,
--                                       but Octo accepts it and builds a cross reference for it.

-- Four rows, chosen so that each of AIM's blind spots is visible:

--   * id 1 : has all three nodes.
--   * id 2 : has ^USER(2,0) only. Both "email" and "dept" are missing-node SQL NULLs.
--   * id 3 : has ^USER(3,"EMAIL") only. "name" and "dept" are missing-node NULLs.
--   * id 4 : has a value on the ROW NODE ITSELF, ^USER(4)="Dave", and no children at all.
--            It is a row by any full-table-scan definition, and all three columns are NULL.

-- The row universe Octo enumerates is $ORDER over ^USER(id), which finds all four. Before AIM had a
-- record specification, a cross reference derived the row from the column's own value node, and the
-- disagreement was a silent wrong answer rather than an error:

--   * by default (AIM type=0) a row without the node had no index entry at all, so WHERE <column>
--     IS NULL found nothing, for any of the three columns.
--   * with AIMTYPE 1 (AIM type=1) the row was the PARENT of the value node. For "name" and "email",
--     one level down, that parent is ^USER(id), the row itself, so they were right. "dept" is two
--     levels down, so its parent level was ^USER(id,1), which only id 1 has: ids 2, 3 and 4 were
--     absent from the index and WHERE dept IS NULL found none of them.

-- With a record specification (AIM type=4) the row universe is ^USER(id) with the levels below it
-- watched, so it matches the full table scan and every one of these rows is found.
CREATE TABLE users (
 id INTEGER PRIMARY KEY,
 name VARCHAR(20) GLOBAL "^USER(keys(""id""),0)" PIECE 1,
 email VARCHAR(30) GLOBAL "^USER(keys(""id""),""EMAIL"")" PIECE 1,
 dept VARCHAR(20) GLOBAL "^USER(keys(""id""),1,""DEPT"")" PIECE 1
) GLOBAL "^USER" AIMTYPE 1 DELIM "^" READONLY;

-- Unfiltered: the row universe from a full table scan. All four ids are rows, including id 4 whose
-- only data is the value on the row node itself.
SELECT * FROM users ORDER BY id;

-- WHERE on "email", a column one level below the row. Correct answer is ids 2 and 4: none by default
-- before this change, both with AIMTYPE 1.
SELECT * FROM users WHERE email IS NULL ORDER BY id;

-- WHERE on "dept", a column TWO levels below the row. Correct answer is ids 2, 3 and 4: none before
-- this change, by default or with AIMTYPE 1, which treated ^USER(id,1) as the row.
SELECT * FROM users WHERE dept IS NULL ORDER BY id;

-- WHERE on "name", one level below the row. Correct answer is ids 3 and 4.
SELECT * FROM users WHERE name IS NULL ORDER BY id;
