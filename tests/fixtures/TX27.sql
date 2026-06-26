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

-- TX27 : YDBAIM#96 : a key fixed by the WHERE clause tests the row node the same way the scan does.

-- Both tables declare exactly one node per row, so a row exists when that node holds a value
-- (see TX23), and a table scan rejects a node that is in the tree only through descendants:
-- its FOR loop carries a "QUIT:'($DATA(<row node>)#10)". A key that the WHERE clause fixes with
-- =, IN or a range has no such loop: it probes the key's node directly. That probe must ask the
-- same question of the same node, or "WHERE id = 2" finds a row that "SELECT *" does not.

-- order_lines: rows at ^ORD(order_id,line_id). (2,1) holds no value; it is in the tree only
-- because of the undeclared child ^ORD(2,1,"NOTE"). Not a row.
CREATE TABLE order_lines (
 order_id INTEGER,
 line_id INTEGER,
 item VARCHAR(20) PIECE 1,
 PRIMARY KEY (order_id, line_id)
) GLOBAL "^ORD" DELIM "^" READONLY;

-- episodes: rows at ^GCND(id,"A"), a constant subscript BELOW the key. For id 2 that node is
-- absent; ^GCND(2) exists only through ^GCND(2,"B"), which the table does not declare. Not a row,
-- and the node to test is ^GCND(2,"A"), not ^GCND(2).
CREATE TABLE episodes (
 id INTEGER PRIMARY KEY,
 note VARCHAR(40) PIECE 1
) GLOBAL "^GCND(keys(""id""),""A"")" DELIM "^" READONLY;

-- The scans: the row universe each table has.
SELECT * FROM order_lines ORDER BY order_id, line_id;
SELECT * FROM episodes ORDER BY id;

-- The same tables with a key fixed by =, by IN and by a range. Every answer must be a subset of
-- the scan above it: (2,1) and id 2 never appear.
SELECT * FROM order_lines WHERE order_id = 2 AND line_id = 1;
SELECT * FROM order_lines WHERE order_id = 2 AND line_id IN (1,2);
SELECT * FROM order_lines WHERE order_id = 2 AND line_id > 0;
SELECT * FROM order_lines WHERE order_id = 1 AND line_id = 2;
SELECT * FROM episodes WHERE id = 2;
SELECT * FROM episodes WHERE id IN (1,2,3);
SELECT * FROM episodes WHERE id >= 2;
SELECT * FROM episodes WHERE id < 3 ORDER BY id DESC;
SELECT * FROM episodes WHERE id = 3;
