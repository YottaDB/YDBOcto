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

-- TX21 : YDBAIM#96 : a column whose GLOBAL maps a PARENT subscript of the primary key.

-- The primary key is (order_id, line_id), so a row is the pair (order_id, line_id). Each row's data
-- lives one level BELOW it, at ^ORD(order_id,line_id,<child>), exactly like the AIMTYPE documentation
-- example where ^EMP(emp_id,0) holds the department and ^EMP(emp_id,"EMAIL") holds the email:

--   * "item"        -> ^ORD(order_id,line_id,0)        : the row's main data (always present).
--   * "ship_method" -> ^ORD(order_id,line_id,"SHIP")   : a LOWER-level node keyed by the fixed
--                                                         subscript "SHIP" (same shape as "email").
--   * "customer"    -> ^ORD(order_id)                  : a HIGHER-level PARENT node, recorded once
--                                                         per order and shared by every line.

-- Two rows have a "missing node" SQL NULL:

--   * Row (2,1): ^ORD(2,1,"SHIP") is absent, so ship_method IS NULL.
--   * Row (3,1): ^ORD(3) is absent, so customer IS NULL.

-- A WHERE on a non-key column makes Octo build an AIM cross reference on that column, with the
-- table's own GLOBAL as the record specification, so a row is a row whether or not the column's
-- node exists. "customer" is the shape AIMTYPE 1 could not index at all: its global ^ORD(order_id)
-- has a single subscript, and type 1 needed at least two (%YDBAIM-F-NOSUBS). The AIMTYPE 1 in the
-- DDL below is accepted and ignored.
CREATE TABLE order_lines (
 order_id INTEGER,
 line_id INTEGER,
 customer VARCHAR(20) GLOBAL "^ORD(keys(""order_id""))" PIECE 1,
 item VARCHAR(20) GLOBAL "^ORD(keys(""order_id""),keys(""line_id""),0)" PIECE 1,
 ship_method VARCHAR(20) GLOBAL "^ORD(keys(""order_id""),keys(""line_id""),""SHIP"")" PIECE 1,
 PRIMARY KEY (order_id, line_id)
) GLOBAL "^ORD" AIMTYPE 1 DELIM "^" READONLY;

-- Unfiltered : all rows are present. Row (2,1) has an empty ship_method (^ORD(2,1,"SHIP") absent);
-- row (3,1) has an empty customer (parent node ^ORD(3) absent).
SELECT * FROM order_lines ORDER BY order_id, line_id;

-- WHERE on "ship_method" (a LOWER-level node with a fixed trailing subscript): returns row (2,1),
-- whose ship_method is a missing-node NULL.
SELECT * FROM order_lines WHERE ship_method IS NULL ORDER BY order_id, line_id;

-- WHERE on "customer" (a PARENT node, a single subscript): returns row (3,1), whose customer is a
-- missing-node NULL.
SELECT * FROM order_lines WHERE customer IS NULL ORDER BY order_id, line_id;
