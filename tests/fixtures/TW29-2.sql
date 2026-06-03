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

-- TW29 Case 2 (YDBOcto#1124) : the column's global lists ALL primary-key columns, but in a DIFFERENT order
-- than the primary key. The primary key is (a, b) but "val" maps to ^GV(keys("b"),keys("a")). Every
-- primary-key subscript is present in the cross reference, so it is used: the keys are advanced in the
-- column's global order (b, then a) so each cross-reference subscript binds to the right column. (Before
-- the #1124 fix this silently returned a row with a and b swapped.)
CREATE TABLE revkeys (
 a INTEGER,
 b INTEGER,
 val VARCHAR(20) GLOBAL "^GV(keys(""b""),keys(""a""))" PIECE 1,
 PRIMARY KEY (a, b)
) GLOBAL "^GROW" READONLY;

-- Unfiltered : confirms the data is present.
SELECT * FROM revkeys ORDER BY a, b;

-- Filtered : WHERE on the reordered-all-keys column. Must match the unfiltered row exactly.
SELECT * FROM revkeys WHERE val = 'hello' ORDER BY a, b;
