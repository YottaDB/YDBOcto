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
-- Run after ^sxwn(1,0) is updated and ^sxwn(2,0) is zkilled directly (the table is READONLY).
-- The AIM trigger re-derives the $EXTRACT'ed value, so the index tracks both changes:
-- id 1's old value '45 Oak Avenu' is gone, its new value '999 New Stre' is found, and
-- id 2's entry disappeared entirely.
SELECT id, address FROM sxwn WHERE address = '999 New Stre';
SELECT id, address FROM sxwn WHERE address = '45 Oak Avenu';
-- The table has AIMTYPE 1, so the ZKILL trigger recorded id 2's now-missing node as NULL.
-- AIM applies the column's transformation function to that NULL, so for a string
-- column the marker is $$strcolval2aimsubs("") = "#" -- the bare "#" subscript in the index
-- dump below, the same marker a plain string column uses. The indexed IS NULL lookup finds it:
SELECT id FROM sxwn WHERE address IS NULL;
