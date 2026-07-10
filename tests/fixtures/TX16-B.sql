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
-- MIN/MAX read straight off the AIM index (the OCTO617 optimization) for both SUBSTR forms.

-- Whole-node: ^sxmm(id,0) is "<fruit>XYZ"; chars 1-5 give Mango/Apple/Cherr -> MIN Apple, MAX Mango:
CREATE TABLE sxmm (
	id INTEGER PRIMARY KEY,
	fruit VARCHAR SUBSTR 1-5 GLOBAL "^sxmm(keys(""id""),0)"
) GLOBAL "^sxmm(keys(""id""))" AIMTYPE 1;
SELECT * FROM sxmm ORDER BY id;
SELECT MIN(fruit) AS lo, MAX(fruit) AS hi FROM sxmm;

-- PIECE-based: ^sxmmp(id) is "x|<fruit>Q|z"; piece 2 chars 1-5 give the same fruits:
CREATE TABLE sxmmp (
	id INTEGER PRIMARY KEY,
	fruit VARCHAR PIECE 2 SUBSTR 1-5
) GLOBAL "^sxmmp(keys(""id""))";
SELECT * FROM sxmmp ORDER BY id;
SELECT MIN(fruit) AS lo, MAX(fruit) AS hi FROM sxmmp;
