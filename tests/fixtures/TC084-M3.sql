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
-- Multi-level SKIP + SKIPCONDITION combined: drop "east" region at the outer
-- loop AND any inner id=2 at the inner loop. The codegen emits a QUIT line in
-- each nested FOR-DO body at its own dot depth.
CREATE TABLE skiptest_m3 (
  region VARCHAR(8)  PRIMARY KEY SKIP '"east"',
  id     INTEGER     KEY NUM 1 SKIPCONDITION "keys(""id"")=2",
  name   VARCHAR(64)
) GLOBAL "^skiptestmulti(keys(""region""),keys(""id""))" READONLY;
SELECT * FROM skiptest_m3 ORDER BY region, id;
