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

-- TW29 Case 2 (YDBOcto#1124, revised by YDBAIM#96) : the column's global lists ALL primary-key columns, but
-- in a DIFFERENT order than the primary key. The primary key is (a, b) but "val" maps to
-- ^GV(keys("b"),keys("a")). Octo declines the cross reference and falls back to a full scan that filters
-- correctly. (Before the #1124 fix this silently returned a row with a and b swapped.)
--
-- #1124 originally accepted this shape, advancing the keys in the column's global order so that each
-- cross-reference subscript bound to the right column. YDBAIM#96 withdraws that: the cross reference now
-- also carries a record anchor on the table's own global, so that a row whose value node is absent is
-- indexed as SQL NULL, and AIM binds the anchor's key subscripts to the value spec's positionally. AIM is
-- handed ":" marks and never learns which column each stands for, so it cannot honour the reordering, and
-- would index ^GV(a,b) for data that lives at ^GV(b,a). Declining costs a full scan and nothing else.
--
-- Contrast Case 4, which also lists every primary-key column but IN primary-key order: that is a
-- full-length leading prefix and still uses the cross reference.
CREATE TABLE revkeys (
 a INTEGER,
 b INTEGER,
 val VARCHAR(20) GLOBAL "^GV(keys(""b""),keys(""a""))" PIECE 1,
 PRIMARY KEY (a, b)
) GLOBAL "^GROW" READONLY;

-- Unfiltered : confirms the data is present.
SELECT * FROM revkeys ORDER BY a, b;

-- Filtered : WHERE on the reordered-all-keys column. Must match the unfiltered row exactly, now via a full
-- scan rather than the cross reference.
SELECT * FROM revkeys WHERE val = 'hello' ORDER BY a, b;
