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
-- Multi-level baseline: composite PRIMARY KEY, no SKIP/SKIPCONDITION.
-- Shows the full ^skiptestmulti contents that TC084-M1..12 will filter.
CREATE TABLE skiptest_m0 (
  region VARCHAR(8)  PRIMARY KEY,
  id     INTEGER     KEY NUM 1,
  name   VARCHAR(64)
) GLOBAL "^skiptestmulti(keys(""region""),keys(""id""))" READONLY;
SELECT * FROM skiptest_m0 ORDER BY region, id;
