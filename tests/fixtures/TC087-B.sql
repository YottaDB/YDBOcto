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
-- TC087 : OCTO763 : PIECE + SUBSTR: the character range applies to the extracted piece value.
-- ^subpc nodes are "name|birthdate|idcode". Row 2 has a short idcode ("C9") and row 3 a short
-- birthdate ("1971"), so ranges past the end come back empty (-> NULL for birthday of row 3).

CREATE TABLE subpc (
	id INTEGER PRIMARY KEY,
	initial VARCHAR PIECE 1 SUBSTR 1,
	birthyear INTEGER PIECE 2 SUBSTR 1-4,
	birthday VARCHAR PIECE 2 SUBSTR 9-10,
	idnum INTEGER PIECE 3 SUBSTR 2-5
) GLOBAL "^subpc(keys(""id""))";
\d subpc;
SELECT * FROM subpc ORDER BY id;
-- Row 3's birthdate "1971" has no characters 9-10, so its birthday is NULL:
SELECT id FROM subpc WHERE birthday IS NULL;

-- An explicit column-level DELIM composes with PIECE + SUBSTR ("X^JOHNDOE" -> piece 2 -> chars 2-4):
CREATE TABLE subpcd (
	id INTEGER PRIMARY KEY,
	part VARCHAR DELIM "^" PIECE 2 SUBSTR 2-4
) GLOBAL "^subpcd(keys(""id""))";
\d subpcd;
SELECT * FROM subpcd ORDER BY id;
