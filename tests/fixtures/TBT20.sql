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
-- OCTO1137: A plain BOOLEAN column (no PIECE or SUBSTR) of an AIMTYPE 1 READONLY table now uses AIM
-- metadata type 3 (it previously hardcoded type 2, ignoring AIMTYPE 1). `flag` lives on a SEPARATE
-- node ^tbt20(id,1); the base node ^tbt20(id,0) marks each row. id 2 has NO (2,1) node (a MISSING
-- node) and id 3 has an EMPTY (3,1) node; both are SQL NULL, indexed under the bare "" subscript, so
-- "WHERE flag IS NULL" returns ids 2 and 3. (Before this fix, type 2 did not index the missing node,
-- so id 2 was missed.)
CREATE TABLE tbt20 (
	id INTEGER PRIMARY KEY,
	name VARCHAR PIECE 1 GLOBAL "^tbt20(keys(""id""),0)",
	flag BOOLEAN GLOBAL "^tbt20(keys(""id""),1)"
) GLOBAL "^tbt20(keys(""id""))" AIMTYPE 1 READONLY;
SELECT * FROM tbt20 ORDER BY id;
SELECT id FROM tbt20 WHERE flag IS NULL ORDER BY id;
SELECT id FROM tbt20 WHERE flag = TRUE ORDER BY id;
SELECT id FROM tbt20 WHERE flag = FALSE ORDER BY id;
