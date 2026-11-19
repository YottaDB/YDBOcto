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

-- TOB28 : OCTO1195 : ORDER BY DESC on a key column with START ends when no node is at the START value
-- An ORDER BY ... DESC on a key column walks the table's global backwards. With a START without STARTINCLUDE on that
-- column, the walk stopped only at a subscript equal to the START value. When the global has no node at that value, the
-- walk went past the first subscript and began again from the last one: the rows repeated up to the LIMIT, and without
-- a LIMIT the query did not end. The walk also returned the nodes before the START value, which are not rows.
-- Each LIMIT below is more than the number of rows the query should return, so that repeated rows would show.

-- START 0: no node of ^TOB28 has a = 0
CREATE TABLE TOB28s0 (a INTEGER START 0, b INTEGER, v VARCHAR PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TOB28(keys(""a""),keys(""b""))" READONLY;
-- The table scan: (1,7,a) (2,3,b) (2,5,f) (4,3,e) (5,3,g)
SELECT * FROM TOB28s0;
-- [no sort] The 3 rows with b = 3: (5,3,g) (4,3,e) (2,3,b)
SELECT * FROM TOB28s0 WHERE b = 3 ORDER BY a DESC LIMIT 5;
-- [no sort] Both key columns descending, all 5 rows: (5,3,g) (4,3,e) (2,5,f) (2,3,b) (1,7,a)
SELECT * FROM TOB28s0 ORDER BY a DESC, b DESC LIMIT 8;

-- START 3: no node has a = 3, and the nodes with a = 1 and a = 2 are before the START, so they are not rows
CREATE TABLE TOB28s3 (a INTEGER START 3, b INTEGER, v VARCHAR PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TOB28(keys(""a""),keys(""b""))" READONLY;
-- The table scan: (4,3,e) (5,3,g)
SELECT * FROM TOB28s3;
-- [no sort] (5,3,g) (4,3,e)
SELECT * FROM TOB28s3 ORDER BY a DESC LIMIT 5;
-- [no sort] No row has b = 5: its only node, (2,5), is before the START
SELECT * FROM TOB28s3 WHERE b = 5 ORDER BY a DESC LIMIT 5;

-- START 2: the nodes with a = 2 are at the START value, which is not a row either. The walk meets it and ends there.
CREATE TABLE TOB28s2 (a INTEGER START 2, b INTEGER, v VARCHAR PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TOB28(keys(""a""),keys(""b""))" READONLY;
-- The table scan: (4,3,e) (5,3,g)
SELECT * FROM TOB28s2;
-- [no sort] (5,3,g) (4,3,e)
SELECT * FROM TOB28s2 ORDER BY a DESC LIMIT 5;

-- START 2 STARTINCLUDE: the nodes with a = 2 are rows, and the node with a = 1 is not
CREATE TABLE TOB28s2i (a INTEGER START 2 STARTINCLUDE, b INTEGER, v VARCHAR PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TOB28(keys(""a""),keys(""b""))" READONLY;
-- The table scan: (2,3,b) (2,5,f) (4,3,e) (5,3,g)
SELECT * FROM TOB28s2i;
-- [no sort] (5,3,g) (4,3,e) (2,5,f) (2,3,b)
SELECT * FROM TOB28s2i ORDER BY a DESC, b DESC LIMIT 8;
