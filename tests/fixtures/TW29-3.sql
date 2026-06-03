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

-- TW29 Case 3 (YDBOcto#1124) : the column's global maps a NON-PREFIX subset of the primary key (it skips a
-- key column). The primary key is (a, b, c) but "val" maps to ^GS(keys("a"),keys("c")), skipping b. Octo
-- declines the cross reference and falls back to a full scan that filters correctly. (Before the #1124 fix
-- this silently returned wrong rows.)
CREATE TABLE gapkeys (
 a INTEGER,
 b INTEGER,
 c INTEGER,
 val VARCHAR(20) GLOBAL "^GS(keys(""a""),keys(""c""))" PIECE 1,
 PRIMARY KEY (a, b, c)
) GLOBAL "^GROWB" READONLY;

-- Unfiltered : confirms the data is present.
SELECT * FROM gapkeys ORDER BY a, b, c;

-- Filtered : WHERE on the gap-subset column. Must match the unfiltered row exactly.
SELECT * FROM gapkeys WHERE val = 'hit' ORDER BY a, b, c;
