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
-- Show the raw, unfiltered contents of ^skipmax (used by TC084-MAX0, TC084-MAX1,
-- TC084-AIM0, TC084-AIM1, TC084-AIM2, TC084-AIMW0, and TC084-AIMW1).
-- Each fixture below maps a SKIP'd or SKIPCONDITION'd subset of these rows --
-- comparing this listing to the SELECT * inside each subsequent fixture makes
-- the effect of SKIP / SKIPCONDITION easy to see in code review.
CREATE TABLE skipmax_show (
  id   INTEGER PRIMARY KEY,
  name VARCHAR(64)
) GLOBAL "^skipmax" READONLY;
SELECT * FROM skipmax_show ORDER BY id;
DROP TABLE skipmax_show;
