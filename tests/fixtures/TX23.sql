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

-- TX23 : YDBAIM#96 : a column stored AT the row's own depth.

-- The primary key is (order_id, line_id), so a row is ^ORD(order_id,line_id). The "item" column has
-- no column-level GLOBAL, so it inherits the table's - the row node itself - and is a piece of it:

--   * "item" -> ^ORD(order_id,line_id) PIECE 1

-- No column of this table carries a GLOBAL of its own, so the schema declares exactly ONE node per
-- row. Octo therefore defines a row here as "that node holds a value", and cross references the
-- column as AIM type 0, which indexes precisely the nodes holding a value. The two definitions are
-- chosen together in tmpl_physical_plan / tmpl_tablejoin and only agree together.

-- What that costs is row (2,1) below. It carries no value and exists solely because of the
-- undeclared child ^ORD(2,1,"NOTE"), so under this rule it is not a row at all - it disappears from
-- the unfiltered SELECT as well as from "WHERE item IS NULL". The alternative was to keep the
-- decoupled record anchor for every table so that (2,1) stays a row with a NULL "item". That is
-- not correct: nothing in the schema declares ^ORD(2,1,"NOTE"), so nothing in the schema makes
-- (2,1) a row. It would also put record-side triggers on every table, firing on every write.

-- The anchor is still used wherever a column can be absent while its row is not: any column below
-- the row, in another global, or in another database. See TX22, TX24 and TX26.
CREATE TABLE order_lines (
 order_id INTEGER,
 line_id INTEGER,
 item VARCHAR(20) PIECE 1,
 PRIMARY KEY (order_id, line_id)
) GLOBAL "^ORD" AIMTYPE 1 DELIM "^" READONLY;

-- Unfiltered: the row universe from a full table scan. Row (2,1) is absent - the one node the
-- schema declares for it, ^ORD(2,1), holds no value.
SELECT * FROM order_lines ORDER BY order_id, line_id;

-- WHERE on "item". No row has a NULL "item", and the answer agrees with the scan above: the
-- cross reference and the full scan share one definition of a row.
SELECT * FROM order_lines WHERE item IS NULL ORDER BY order_id, line_id;
