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
-- AIM-xref path with a NULL column value indexed by AIM. ^skipnull is seeded
-- with rows {1:apple, 2:"", 3:zebra}. Row 2 has an empty name; AIM stores
-- string columns under the "#"+colvalue subscript, so the empty-name row
-- ends up under "#" (the AIM NULL representation).  The DO body's
-- QUIT:""=$$aimsubs2strcolval(%ydboctoexpr) line decodes "#" back to "" and
-- exits the DO body, leaving the key column's LVN at "" so the outer FOR sees
-- "no keeper here" and re-orders past this col_value.  This is how the AIM
-- optimization implements the SQL rule that aggregates ignore NULL.
--
-- With SKIP '3' on id, row 3 (zebra) is filtered out. Row 2 has a NULL name
-- which the AIM NULL-rep QUIT skips. So MAX(name) and MIN(name) should both
-- return 'apple' (the only row with a non-NULL, non-SKIP'd name).
CREATE TABLE skiptest_aim4 (
  id   INTEGER PRIMARY KEY SKIP '3',
  name VARCHAR(64)
) GLOBAL "^skipnull";
SELECT * FROM skiptest_aim4 ORDER BY id;
SELECT max(name) FROM skiptest_aim4;
SELECT min(name) FROM skiptest_aim4;
