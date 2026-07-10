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
-- PIECE + SUBSTR: AIM extracts the single piece it is given (delimiter "|", piece 2) and the
-- SubstrXform transformation function slices chars 2-4 out of it before indexing.
-- ^sxpc nodes are "name|<letter><digits>"; codes X12345/Y67890/X12399 -> 123/678/123.
CREATE TABLE sxpc (
	id INTEGER PRIMARY KEY,
	name VARCHAR PIECE 1,
	code VARCHAR PIECE 2 SUBSTR 2-4
) GLOBAL "^sxpc(keys(""id""))";
SELECT * FROM sxpc ORDER BY id;
-- Expect ids 0 and 2 (codes X12345 and X12399 both slice to '123'):
SELECT id, code FROM sxpc WHERE code = '123' ORDER BY id;

-- AIMTYPE 1 + PIECE + SUBSTR on column-level GLOBALs, for every data type. Every ^sxpcg row has
-- its (id,0) node; id 2 has NONE of the four data nodes (missing nodes) and id 3 has all four
-- nodes but each extraction comes back empty (a missing piece or a range past the end). Both
-- flavors are SQL NULL, indexed under the bare "" subscript (AIM metadata type 3), so every
-- indexed IS NULL lookup returns ids 2 and 3; MIN/MAX skip them:
CREATE TABLE sxpcg (
	id INTEGER PRIMARY KEY,
	rowname VARCHAR PIECE 1 GLOBAL "^sxpcg(keys(""id""),0)",
	tag VARCHAR PIECE 2 SUBSTR 2-4 GLOBAL "^sxpcg(keys(""id""),1)",
	dob DATE PIECE 2 SUBSTR 1-10 GLOBAL "^sxpcg(keys(""id""),2)",
	flag BOOLEAN PIECE 2 SUBSTR 2-4 GLOBAL "^sxpcg(keys(""id""),3)",
	qty INTEGER PIECE 2 SUBSTR 4-5 GLOBAL "^sxpcg(keys(""id""),4)"
) GLOBAL "^sxpcg(keys(""id""))" AIMTYPE 1;
SELECT * FROM sxpcg ORDER BY id;
SELECT id FROM sxpcg WHERE tag IS NULL ORDER BY id;
SELECT id FROM sxpcg WHERE dob IS NULL ORDER BY id;
SELECT id FROM sxpcg WHERE flag IS NULL ORDER BY id;
SELECT id FROM sxpcg WHERE qty IS NULL ORDER BY id;
SELECT id, tag FROM sxpcg WHERE tag = '111';
SELECT MIN(qty) AS lo, MAX(qty) AS hi FROM sxpcg;
