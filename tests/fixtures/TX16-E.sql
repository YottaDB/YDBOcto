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
-- DELIMS/PIECES + SUBSTR: the piece-of-piece transform carries the SUBSTR specification as its
-- trailing argument, so AIM indexes the $EXTRACT of the innermost extracted piece.
-- ^sxpp nodes are "x|<street>^<city>|z"; city chars 1-3 give Bos/Ren/Boi. Expect id 2 for 'Boi'.
CREATE TABLE sxpp (
	id INTEGER PRIMARY KEY,
	city3 VARCHAR DELIMS ("|","^") PIECES (2,2) SUBSTR 1-3
) GLOBAL "^sxpp(keys(""id""))";
SELECT * FROM sxpp ORDER BY id;
SELECT id, city3 FROM sxpp WHERE city3 = 'Boi';

-- AIMTYPE 1 + DELIMS/PIECES + SUBSTR on column-level GLOBALs, for every data type (the transform
-- is the 7-argument PieceOfPieceXform). Every ^sxppg row has its (id,0) node; id 2 has NONE of
-- the four data nodes (missing nodes, indexed as NULL by AIM metadata type 3) and id 3 has all
-- four nodes but each innermost piece/range comes back empty (empty extractions). Both flavors
-- are SQL NULL, indexed under the bare "" subscript, so every indexed IS NULL lookup returns
-- ids 2 and 3:
CREATE TABLE sxppg (
	id INTEGER PRIMARY KEY,
	rowname VARCHAR PIECE 1 GLOBAL "^sxppg(keys(""id""),0)",
	city3 VARCHAR DELIMS ("|","^") PIECES (2,2) SUBSTR 1-3 GLOBAL "^sxppg(keys(""id""),1)",
	dob DATE DELIMS ("|","^") PIECES (2,2) SUBSTR 1-10 GLOBAL "^sxppg(keys(""id""),2)",
	flag BOOLEAN DELIMS ("|","^") PIECES (2,2) SUBSTR 2-4 GLOBAL "^sxppg(keys(""id""),3)",
	qty INTEGER DELIMS ("|","^") PIECES (2,2) SUBSTR 4-5 GLOBAL "^sxppg(keys(""id""),4)"
) GLOBAL "^sxppg(keys(""id""))" AIMTYPE 1;
SELECT * FROM sxppg ORDER BY id;
SELECT id, city3 FROM sxppg WHERE city3 = 'Rya';
SELECT id FROM sxppg WHERE city3 IS NULL ORDER BY id;
SELECT id FROM sxppg WHERE dob IS NULL ORDER BY id;
SELECT id FROM sxppg WHERE flag IS NULL ORDER BY id;
SELECT id FROM sxppg WHERE qty IS NULL ORDER BY id;
SELECT MIN(qty) AS lo, MAX(qty) AS hi FROM sxppg;
