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
-- Run after ^ux is changed directly (the table is READONLY). Until now only AIM's initial
-- SCAN had seen multi-byte data; these queries exercise the TRIGGER path, which is separate
-- code in %YDBAIM and re-derives the indexed value by calling SubstrXform itself. The trigger
-- must land on the same character boundary the scan did.
--
--   SET   ^ux(1) : "Xé日Y" -> "東京都", so the indexed extraction changes from Xé日 to 東京都
--   ZKILL ^ux(2) : the "cjk" row goes away, so 日本語 is no longer indexed

-- The old extraction is gone and the new one is found; both literals are multi-byte, so a
-- trigger that counted bytes instead of characters would fail to match either.
select label from ux where head3 = 'Xé日';
select label from ux where head3 = '東京都';

-- The zkilled row is no longer indexed under its old value.
select label from ux where head3 = '日本語';

-- Whole-table view after both changes.
select label, head3 from ux order by label;

-- MIN/MAX re-read off the maintained index. 東(0xE6...) sorts above a(0x61), so the maximum
-- is now 東京都 and the minimum is the remaining ASCII row.
select MIN(head3) as LO, MAX(head3) as HI from ux;
