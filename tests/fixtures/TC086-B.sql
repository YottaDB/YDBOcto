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
-- Deeper nesting: 3-level and 4-level chains. Each ^addrdeep node is "A|B^C~D:E:TARGETn~G^H|I", so
-- p3 = $P($P($P(node,"|",2),"^",2),"~",2) = "D:E:TARGETn" and p4 applies one more ":" level = "TARGETn".
CREATE TABLE addr_deep (
	id INTEGER PRIMARY KEY,
	p3 VARCHAR(40) DELIMS ("|","^","~") PIECES (2,2,2),
	p4 VARCHAR(40) DELIMS ("|","^","~",":") PIECES (2,2,2,3)
) GLOBAL "^addrdeep(keys(""id""))";
\d addr_deep;
SELECT * FROM addr_deep ORDER BY id;
