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
-- Show the raw, unfiltered contents of ^skipnull (used by TC084-AIM4).
-- Row 2 has an empty `name` -- AIM indexes that under the "#" NULL
-- representation subscript and the AIM-SKIP loop body's NULL-rep QUIT is
-- responsible for skipping it during MAX/MIN computation.
CREATE TABLE skipnull_show (
  id   INTEGER PRIMARY KEY,
  name VARCHAR(64)
) GLOBAL "^skipnull" READONLY;
SELECT * FROM skipnull_show ORDER BY id;
DROP TABLE skipnull_show;
