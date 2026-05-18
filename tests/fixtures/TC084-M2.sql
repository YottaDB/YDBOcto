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
-- Multi-level SKIPCONDITION: drop any row where the inner id equals 2.
CREATE TABLE skiptest_m2 (
  region VARCHAR(8)  PRIMARY KEY,
  id     INTEGER     KEY NUM 1 SKIPCONDITION "keys(""id"")=2",
  name   VARCHAR(64)
) GLOBAL "^skiptestmulti(keys(""region""),keys(""id""))" READONLY;
SELECT * FROM skiptest_m2 ORDER BY region, id;
