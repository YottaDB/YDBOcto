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

-- TX26 : YDBAIM#96 : the rows and a column in DIFFERENT DATABASES.

-- TX24 split the column away from the rows by GLOBAL NAME (^G versus ^H). This splits them by
-- DATABASE as well, which is the case Octo will actually meet in the field: a table whose rows
-- are local but one of whose columns is fetched through an extended reference.

--   * rows       -> ^G(keys("order_id"),keys("line_id"))       : this database
--   * "cust"     -> ^["extended.gld"]H(keys("order_id"))       : another database, and keyed by
--                                                                only the FIRST key column

-- TX12 already covers a table that lives ENTIRELY in an extended-reference database; there the
-- cross reference simply follows the table and one database holds everything. Here the two are
-- genuinely apart, so whichever database the cross reference lands in, its triggers have to reach
-- across to the other one - the value trigger to ask whether a row still exists, the record
-- trigger to read the column.

-- This needs a record specification for the same reason TX24 does: writing ^H says nothing about
-- which rows exist, so AIM cannot derive the row from the column's own global. (AIMTYPE 1 failed
-- here as on TX24's "cust", with %YDBAIM-F-NOSUBS: one subscript where it demanded two.)

-- The data gives "cust" a genuine missing-node NULL: there is no ^H(2) in the extended database
-- at all, so order 2's rows must come back with cust NULL rather than being dropped.
CREATE TABLE xorder_lines (
 order_id INTEGER,
 line_id INTEGER,
 cust VARCHAR(20) GLOBAL "^[""extended.gld""]H(keys(""order_id""))" PIECE 1,
 PRIMARY KEY (order_id, line_id)
) GLOBAL "^G" AIMTYPE 1 DELIM "^" READONLY;

-- Unfiltered: three rows, all from the LOCAL ^G. cust is Alice for order 1's two lines and NULL
-- for order 2, whose ^H(2) does not exist in the extended database.
SELECT * FROM xorder_lines ORDER BY order_id, line_id;

-- WHERE on the column that lives in the other database. Correct answer is (2,1).
SELECT * FROM xorder_lines WHERE cust IS NULL ORDER BY order_id, line_id;

-- And the complement, which must find the two rows of order 1.
SELECT * FROM xorder_lines WHERE cust = 'Alice' ORDER BY order_id, line_id;
