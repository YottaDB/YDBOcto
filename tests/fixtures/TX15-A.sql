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
-- ^ppaddr nodes are "name|street^city|phone"; street/city are extracted two levels deep.
-- Row id 3 ("Neo|^Zion|p3") has an EMPTY "^"-subpiece for street, so its street extracts to NULL.
CREATE TABLE ppaddr (
	id INTEGER PRIMARY KEY,
	street VARCHAR DELIMS ("|","^") PIECES (2,1),
	city VARCHAR DELIMS ("|","^") PIECES (2,2)
) GLOBAL "^ppaddr(keys(""id""))";
-- Extracted view (no conditions):
SELECT * FROM ppaddr ORDER BY id;
-- WHERE builds/uses the AIM cross-reference (expect id 1):
SELECT * FROM ppaddr WHERE street = '45 Oak';
-- Empty extracted subpiece -> NULL: IS NULL finds id 3, IS NOT NULL excludes it:
SELECT id, street, city FROM ppaddr WHERE street IS NULL;
SELECT id, street, city FROM ppaddr WHERE street IS NOT NULL ORDER BY id;
