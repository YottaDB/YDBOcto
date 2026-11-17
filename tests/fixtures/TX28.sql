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

# TX28 : OCTO1167 : A query that follows a cross reference build that stopped on an error is not reported as canceled
# (The test prints that line first; lines that start with "#" are not passed to octo.)

-- "v" of row 1 is 1100 bytes long, more than the subscripts of a node of the AIM region can hold. The WHERE below
-- builds the cross reference of "v", and the build stops with %YDB-E-GVSUBOFLOW at that row.
CREATE TABLE TX28 (id INTEGER PRIMARY KEY, v VARCHAR PIECE 1) GLOBAL "^TX28(keys(""id""))" READONLY;
SELECT id FROM TX28 WHERE v = 'b';
-- The next query of the same session: both rows. Before the fix, it failed with
-- "ERR_QUERY_CANCELED: canceling statement due to user request".
SELECT id FROM TX28;
-- The next query ran normally before the fix.
SELECT id FROM TX28;
