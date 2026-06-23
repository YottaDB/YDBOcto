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
-- AIMTYPE 1 + DATE -> AIM type 3. "dob" lives on a SEPARATE node ^v3(id,"D"); the base node ^v3(id,0)
-- marks each row. id 1 has NO "D" node, so its dob is a MISSING node, which type 3 indexes as NULL
-- (so "WHERE dob IS NULL" finds id 1).
CREATE TABLE v3 (
	id INTEGER PRIMARY KEY,
	dob DATE GLOBAL "^v3(keys(""id""),""D"")" DELIMS ("|","^") PIECES (2,1)
) GLOBAL "^v3(keys(""id""))" AIMTYPE 1 READONLY;
SELECT * FROM v3 ORDER BY id;
SELECT * FROM v3 WHERE dob = date'2020-01-15' ORDER BY id;
SELECT * FROM v3 WHERE dob IS NULL ORDER BY id;

-- Same, but the dob value is 3 levels deep within ^v33(id,"D") = "a|b^c~<date>~e^f|g".
CREATE TABLE v33 (
	id INTEGER PRIMARY KEY,
	dob DATE GLOBAL "^v33(keys(""id""),""D"")" DELIMS ("|","^","~") PIECES (2,2,2)
) GLOBAL "^v33(keys(""id""))" AIMTYPE 1 READONLY;
SELECT * FROM v33 ORDER BY id;
SELECT * FROM v33 WHERE dob = date'2020-01-15' ORDER BY id;
SELECT * FROM v33 WHERE dob IS NULL ORDER BY id;
