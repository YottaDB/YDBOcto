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

-- TX24 : YDBAIM#96 : columns stored in a DIFFERENT global from the rows.

-- The primary key is (order_id, line_id) and the rows live in ^G. Neither column lives there:

--   * "cust" -> ^H(keys("order_id"))                    : a different global, and keyed by only the
--                                                         FIRST key column, so one value is shared
--                                                         by every line of an order.
--   * "note" -> ^H(keys("order_id"),keys("line_id"))    : a different global at the same key depth
--                                                         as the row, one value per row.

-- Writing either node cannot bring a row into existence - rows are in ^G - so a column global here
-- says nothing at all about which rows exist. That is precisely what a decoupled record anchor is
-- for, and it is the pair of cases YDBAIM#96 calls cases 4 and 3.

-- AIMTYPE 1 derived its idea of a row from the COLUMN's own global, so both failed, and they
-- failed differently:

--   * "cust" has one subscript, and type=1 required at least two   -> %YDBAIM-F-NOSUBS
--   * "note" has two, but the last one varies rather than being a
--     fixed literal, which type=1 also required                    -> %YDBAIM-F-SUBERR1

-- With the table's GLOBAL as the record specification, both are indexed.

-- The data below gives each column a genuine missing-node NULL: order 2 has no ^H(2) at all, and
-- only row (1,1) has a note.
CREATE TABLE order_lines (
 order_id INTEGER,
 line_id INTEGER,
 cust VARCHAR(20) GLOBAL "^H(keys(""order_id""))" PIECE 1,
 note VARCHAR(30) GLOBAL "^H(keys(""order_id""),keys(""line_id""))" PIECE 1,
 PRIMARY KEY (order_id, line_id)
) GLOBAL "^G" AIMTYPE 1 DELIM "^" READONLY;

-- Unfiltered: three rows, all in ^G. cust is NULL for order 2; note is NULL for all but (1,1).
SELECT * FROM order_lines ORDER BY order_id, line_id;

-- WHERE on "cust", a different global keyed by a PREFIX of the primary key. Correct answer is
-- (2,1).
SELECT * FROM order_lines WHERE cust IS NULL ORDER BY order_id, line_id;

-- WHERE on "note", a different global at the row's own key depth. Correct answer is (1,2) and
-- (2,1).
SELECT * FROM order_lines WHERE note IS NULL ORDER BY order_id, line_id;
