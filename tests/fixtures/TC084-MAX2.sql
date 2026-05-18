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
-- SKIP and SKIPCONDITION combined on the key column, exercised through
-- YDBOcto#617's key-column MAX/MIN one-shot $ORDER path (not the AIM-xref
-- path; the AIM-xref path with both filters is exercised separately in
-- TC084-AIM2 via max(name)/min(name)).  A value is filtered when it appears
-- in the SKIP list OR matches the SKIPCONDITION predicate -- the OCTO617
-- loop QUITs only on a value that passes both filters (a "keeper").
--
-- ^skipmax = {1, 2, 50, 99, 100}.  SKIP '50' filters {50};
-- SKIPCONDITION "keys(""id"")>90" filters {99, 100}.  Together they remove
-- {50, 99, 100}, leaving {1, 2}.  So MAX(id) = 2 and MIN(id) = 1 -- pre-
-- filter MAX(id) = 100, so the MAX result is the distinctive proof that
-- both filter forms are being applied (an implementation that honored only
-- SKIP would still return 100; one that honored only SKIPCONDITION would
-- return 50; only the correct combined behavior returns 2).
CREATE TABLE skiptest_max2 (
  id   INTEGER PRIMARY KEY SKIP '50' SKIPCONDITION "keys(""id"")>90",
  name VARCHAR(64)
) GLOBAL "^skipmax";
SELECT * FROM skiptest_max2 ORDER BY id;
SELECT max(id) FROM skiptest_max2;
SELECT min(id) FROM skiptest_max2;
SELECT max(id), min(id) FROM skiptest_max2;
