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
-- Forward keys() reference: SKIPCONDITION is on "region" (KEY NUM 0) but references
-- keys("id") where "id" is KEY NUM 1. At the point the region-level QUIT fires, the
-- inner $ORDER has not yet set the "id" key's local-variable slot, so a runtime
-- LVUNDEF would occur. Expect ERR_KEYS_FORWARD_REFERENCE at CREATE TABLE time.
CREATE TABLE skiptest_err2 (
  region VARCHAR(8)  PRIMARY KEY SKIPCONDITION "keys(""id"")=2",
  id     INTEGER     KEY NUM 1,
  name   VARCHAR(64)
) GLOBAL "^skiptestmulti(keys(""region""),keys(""id""))" READONLY;
