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

-- TBT19: OCTO1131 regression. `ForceBoolean^%ydboctoplanhelpers` must preserve SQL NULL.
-- A BOOLEAN column of a READONLY table is run through `$$ForceBoolean` in the generated plan
-- (the `is_boolean_column && !table->readwrite` branch of tmpl_column_reference). `savail`
-- below is that READONLY table, mapped over the same `^stockAvailable` global as the readwrite
-- `stock_availability` (so the readwrite version does NOT reproduce this -- it never emits
-- ForceBoolean).
-- `alias1.available > TRUE` can never be true (nothing is greater than TRUE), so the LEFT JOIN
-- never matches and `alias1` is always NULL-extended: `alias1.available` is $ZYSQLNULL on every
-- row. `alias1.available IS NOT NULL` must therefore be FALSE (`f`) for all 8 rows.
-- The plan applies `$$ForceBoolean` to that $ZYSQLNULL value at an *unwrapped* call site (no
-- trailing `SET:(""=expr) expr=$ZYSQLNULL` guard). Before ForceBoolean was fixed to preserve
-- SQL NULL it returned a plain empty string for $ZYSQLNULL, so `'$ZYISSQLNULL("")` was TRUE and
-- every row wrongly printed `t`.
CREATE TABLE savail (product_id INTEGER PRIMARY KEY, available BOOLEAN) GLOBAL "^stockAvailable" READONLY;
SELECT alias1.available IS NOT NULL AS notnull FROM savail LEFT JOIN savail alias1 ON (alias1.available > TRUE);
