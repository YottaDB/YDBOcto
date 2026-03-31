#################################################################
#								#
# Copyright (c) 2025-2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TC081 : OCTO1101 : values() correctly resolves references to EXTRACT columns

DROP TABLE IF EXISTS test;
CREATE TABLE test (
	id integer primary key,
	f1 varchar PIECE 1,
	ex1 varchar EXTRACT "$E(^test(keys(""id"")),1)",
	ex2 varchar EXTRACT "$E(^test(keys(""id"")),2)",
	v1 varchar EXTRACT "values(""ex1"")",
	v2 varchar EXTRACT "values(""ex2"")",
	v12 varchar EXTRACT "values(""ex1"")_"".""_values(""ex2"")"
) GLOBAL "^test";

select * from test;
