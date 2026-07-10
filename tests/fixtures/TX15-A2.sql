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
-- Run after ^ppaddr(1) is updated and ^ppaddr(2) is zkilled directly (the table is READONLY).
-- The AIM trigger re-derives the chained value, so the index tracks both kinds of change:
--   SET  : id 1's old street '45 Oak' is gone and its new street '999 New St' is found;
--   ZKILL: id 2's node is gone, so its street '9 Elm' is no longer indexed and the row
--          disappears from the table altogether (the whole node was removed, not just the
--          extracted piece, so this is a row deletion rather than a transition to NULL).
SELECT * FROM ppaddr WHERE street = '45 Oak';
SELECT * FROM ppaddr WHERE street = '999 New St';
SELECT * FROM ppaddr WHERE street = '9 Elm';
SELECT * FROM ppaddr ORDER BY id;
