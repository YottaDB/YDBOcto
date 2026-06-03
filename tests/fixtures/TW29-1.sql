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

-- TW29 Case 1 (YDBOcto#1124) : the column's global maps a LEADING PREFIX of the primary key, in
-- primary-key order. "customer" is recorded once per order at the shallower node ^ORD(order_id); each line
-- item is a deeper node ^ORD(order_id,line_id). The cross reference advances order_id (its only key); the
-- remaining primary-key column line_id is advanced from the table's own global ^ORD(order_id,line_id).
-- This is the case that returned 0 rows before the #1124 fix.
CREATE TABLE order_lines (
 order_id INTEGER,
 line_id INTEGER,
 customer VARCHAR(20) GLOBAL "^ORD(keys(""order_id""))" PIECE 1,
 item VARCHAR(20) PIECE 1,
 PRIMARY KEY (order_id, line_id)
) GLOBAL "^ORD" READONLY;

-- Unfiltered : confirms the data is present (customer resolved per order).
SELECT * FROM order_lines ORDER BY order_id, line_id;

-- Filtered : WHERE on the cross-referenced column. Expect the two order 1 rows.
SELECT * FROM order_lines WHERE customer = 'Alice' ORDER BY order_id, line_id;
