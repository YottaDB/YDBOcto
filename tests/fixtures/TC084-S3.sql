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
-- Quote-aware comma parsing: the comma inside "x,y" must NOT be treated as a
-- list separator. The list below has two elements: "x,y" and "C".
CREATE TABLE skiptest_s3 (
  id VARCHAR(16) PRIMARY KEY SKIP '"x,y","C"',
  name VARCHAR(64)
) GLOBAL "^skiptest" READONLY;
SELECT * FROM skiptest_s3 ORDER BY id;
