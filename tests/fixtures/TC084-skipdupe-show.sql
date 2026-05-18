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
-- Show the raw, unfiltered contents of ^skipdupe (used by TC084-AIM3).
-- The shape of this seed -- multiple keys per col_value, two col_values whose
-- keys are all SKIP'd by TC084-AIM3 -- is what stresses the AIM-xref inner-FOR
-- and outer-FOR re-ordering.
CREATE TABLE skipdupe_show (
  id   INTEGER PRIMARY KEY,
  name VARCHAR(64)
) GLOBAL "^skipdupe" READONLY;
SELECT * FROM skipdupe_show ORDER BY id;
DROP TABLE skipdupe_show;
