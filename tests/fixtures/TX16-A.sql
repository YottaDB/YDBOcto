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
-- ^sxwn(id) is the name; the address lives on its own ^sxwn(id,0) node. The address column is the
-- whole-node SUBSTR form, so AIM indexes the ENTIRE ^sxwn(id,0) node (empty separator, no piece
-- number) through the SubstrXform transformation function.
CREATE TABLE sxwn (
	id INTEGER PRIMARY KEY,
	name VARCHAR PIECE 1,
	address VARCHAR SUBSTR 1-12 GLOBAL "^sxwn(keys(""id""),0)"
) GLOBAL "^sxwn(keys(""id""))" AIMTYPE 1;
-- Extracted view (no conditions):
SELECT * FROM sxwn ORDER BY id;
-- WHERE builds/uses the AIM cross-reference (expect id 0):
SELECT id, address FROM sxwn WHERE address = '123 Main Str';
