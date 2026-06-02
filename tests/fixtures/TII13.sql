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

-- TII13 : OCTO1121 : INSERT type-mismatch error names the column that actually mismatched.
-- Before the fix the error always named the first column of the INSERT column list (and that
-- column's type), regardless of which column actually had the type mismatch, because it used the
-- loop iterator (which had wrapped back to the first column) instead of the recorded mismatch column.

CREATE TABLE TII13 (name VARCHAR(20), id INTEGER, flag BOOLEAN);

-- Mismatch on the 3rd column "flag": expect 'flag' / BOOLEAN, not 'name' / VARCHAR.
INSERT INTO TII13 (name, id, flag) VALUES ('abc', 5, 6);

-- Mismatch on the 2nd column "id": expect 'id' / INTEGER, not 'name' / VARCHAR.
INSERT INTO TII13 (name, id, flag) VALUES ('abc', 'notanumber', true);

-- Mismatch on the 1st column "name" (control case): should already report 'name' correctly.
INSERT INTO TII13 (name, id, flag) VALUES (6, 5, true);
