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
-- TITER15 : Section A : bare-entryref ITERATOR reports its own table and key column
DROP TABLE IF EXISTS titer15a;
CREATE TABLE titer15a
(
	catsys INTEGER ITERATOR "$$catsys^TITER15",
	id INTEGER ITERATOR "$$id^TITER15",
	iterctx VARCHAR(100) EXTRACT "$$seen^TITER15($$tableName^%ydboctoplanhelpers())",
	extractctx VARCHAR(100) EXTRACT "$$whoami^TITER15",
	PRIMARY KEY (catsys,id)
)
GLOBAL "^TITER15G(keys(""catsys""),keys(""id""))";
SELECT * FROM titer15a;
