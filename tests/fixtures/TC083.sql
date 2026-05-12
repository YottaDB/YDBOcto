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
-- TC083 : OCTO1110 : values() correctly resolved referneces to PIECE columns with GLOBAL
DROP TABLE IF EXISTS test;
CREATE TABLE test (
	id integer PRIMARY KEY,
	f1 varchar PIECE 1,
	f2 varchar GLOBAL "^test(keys(""id""),1)" PIECE 1,
	v1 varchar EXTRACT "$EXTRACT(values(""f2""))"
) GLOBAL "^test";

select * from test;
