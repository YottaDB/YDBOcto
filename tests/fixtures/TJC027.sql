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

-- TJC027 : OCTO1167 : No ERR_QUERY_CANCELED for a query whose first plan uses a cross reference that the plan run does not build
-- The local node %ydboctoCancel(table,column,"aimXref") was set when a plan that uses a new cross reference was
-- generated, and removed when the plan ran and built the cross reference. Any node under %ydboctoCancel is taken for
-- a cancel, so a query whose plan did not run that build was reported as canceled (SQLSTATE 57014) though no one
-- canceled it. Here that happened with "id LIKE ? ORDER BY a": the plan of the Parse message walks the cross reference
-- of column a for the ORDER BY, and for a bound value without a wildcard, Bind generates another plan (a key fix), which
-- builds none.

CREATE TABLE TJC027 (id VARCHAR PRIMARY KEY, a INTEGER);
INSERT INTO TJC027 VALUES ('a', 8), ('ab', 2), ('abc', 1), ('b', 3);
SELECT * FROM TJC027;
