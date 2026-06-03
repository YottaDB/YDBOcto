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

-- TW29 Case 5 (YDBOcto#1124) : the column's global uses a DIFFERENT global name than the table's rows, and
-- lists a LEADING PREFIX of the primary key. The rows live in ^O(cust_id,order_id); "region" is a
-- per-customer attribute in a separate global ^CUSTREGION(cust_id). cust_id is advanced from the cross
-- reference (built on ^CUSTREGION), while the remaining primary-key column order_id is advanced from the
-- table's own global ^O(cust_id,order_id).
CREATE TABLE orders2 (
 cust_id INTEGER,
 order_id INTEGER,
 region VARCHAR(20) GLOBAL "^CUSTREGION(keys(""cust_id""))" PIECE 1,
 amount VARCHAR(10) PIECE 1,
 PRIMARY KEY (cust_id, order_id)
) GLOBAL "^O" READONLY;

-- Unfiltered : confirms the data is present.
SELECT * FROM orders2 ORDER BY cust_id, order_id;

-- Filtered : WHERE on the column stored in a different global (leading-prefix coverage).
SELECT * FROM orders2 WHERE region = 'West' ORDER BY cust_id, order_id;
