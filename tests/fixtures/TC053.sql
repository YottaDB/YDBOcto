#################################################################
#								#
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TC053 : OCTO863 : Parens in EXTRACT fields do not affect generated M code parens
DROP TABLE IF EXISTS TEST;
CREATE TABLE TEST(
 ID INTEGER PRIMARY KEY,
 FIELD1 INTEGER EXTRACT "$$pgTableIsVisible^%ydboctopgfunctions(""("")"
);
SELECT FIELD1 FROM TEST;
