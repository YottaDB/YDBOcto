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
-- AIMTYPE 1 + DELIMS/PIECES (no SUBSTR), for every data type -- the IS NULL matrix (YDBOcto#1108 fix
-- riding on #763/#764). Every ^ppnul row has its (id,0) node; id 2 has NONE of the four typed data
-- nodes (MISSING nodes, indexed as NULL by AIM metadata type 3) and id 3 has all four nodes but each
-- inner piece comes back EMPTY (empty piece-of-piece extraction). Both flavors are SQL NULL, indexed
-- under the bare "" subscript, so every indexed IS NULL lookup returns ids 2 and 3 and MIN/MAX skip
-- both. (Before this commit these string/boolean/integer piece-of-piece columns used AIM type 2 and
-- indexed an empty piece under "#", so IS NULL missed the missing-node rows.)
CREATE TABLE ppnul (
	id INTEGER PRIMARY KEY,
	rowname VARCHAR PIECE 1 GLOBAL "^ppnul(keys(""id""),0)",
	city VARCHAR DELIMS ("|","^") PIECES (2,2) GLOBAL "^ppnul(keys(""id""),1)",
	dob DATE DELIMS ("|","^") PIECES (2,2) GLOBAL "^ppnul(keys(""id""),2)",
	flag BOOLEAN DELIMS ("|","^") PIECES (2,2) GLOBAL "^ppnul(keys(""id""),3)",
	qty INTEGER DELIMS ("|","^") PIECES (2,2) GLOBAL "^ppnul(keys(""id""),4)"
) GLOBAL "^ppnul(keys(""id""))" AIMTYPE 1 READONLY;
SELECT * FROM ppnul ORDER BY id;
SELECT id, city FROM ppnul WHERE city = 'Kyoto';
SELECT id FROM ppnul WHERE city IS NULL ORDER BY id;
SELECT id FROM ppnul WHERE dob IS NULL ORDER BY id;
SELECT id FROM ppnul WHERE flag IS NULL ORDER BY id;
SELECT id FROM ppnul WHERE qty IS NULL ORDER BY id;
SELECT MIN(qty) AS lo, MAX(qty) AS hi FROM ppnul;
