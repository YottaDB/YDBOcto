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
-- TITER10 : ORDER BY on a column with ITERATOR is not supported
CREATE TABLE names (id INTEGER PRIMARY KEY ITERATOR "$$id^TITER10", firstName VARCHAR(30), lastName VARCHAR(30)) GLOBAL "^names";

-- VALID CASE: ORDER BY on a non-ITERATOR column of an ITERATOR table returns sorted results
SELECT firstName, lastName FROM names ORDER BY firstName;
-- VALID CASE: ORDER BY DESC on a non-ITERATOR column also works
SELECT firstName FROM names ORDER BY firstName DESC;

-- INVALID CASES
-- ORDER BY DESC on an ITERATOR column should error (was previously generating invalid M syntax)
SELECT * FROM names ORDER BY id DESC;
-- ORDER BY ASC on an ITERATOR column should also error (was previously silently dropping ORDER BY)
SELECT * FROM names ORDER BY id ASC;
-- ORDER BY by column number referencing the ITERATOR column should error too
SELECT * FROM names ORDER BY 1 DESC;
-- ORDER BY using a SELECT-list alias that resolves to the ITERATOR column should error
SELECT id AS my_id FROM names ORDER BY my_id;
-- ORDER BY referencing an ITERATOR column NOT in the SELECT list should also error (no SELECT-list rebinding)
SELECT firstName FROM names ORDER BY id;
-- Composite key with one ITERATOR column: ORDER BY the ITERATOR column should error
CREATE TABLE composite (a INTEGER ITERATOR "$$a^TITER10", b INTEGER, val VARCHAR(30), PRIMARY KEY (a, b)) GLOBAL "^composite(keys(""a""),keys(""b""))";
SELECT * FROM composite ORDER BY a;
