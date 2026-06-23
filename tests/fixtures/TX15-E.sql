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
-- A 3-level-deep column: AIM indexes the inner "|" piece and the transform applies the two outer levels
-- ("^" then "~"). Each ^ppv3 node is "A|B^C~<v>~G^H|I", so v = $P($P($P(node,"|",2),"^",2),"~",2).
-- Expect ids 0 and 2 for 'Alpha'.
CREATE TABLE ppv3 (id INTEGER PRIMARY KEY, v VARCHAR DELIMS ("|","^","~") PIECES (2,2,2)) GLOBAL "^ppv3(keys(""id""))";
SELECT * FROM ppv3 ORDER BY id;
SELECT * FROM ppv3 WHERE v = 'Alpha' ORDER BY id;
