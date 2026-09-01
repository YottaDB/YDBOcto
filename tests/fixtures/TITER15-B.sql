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
-- TITER15 : Section B : ITERATOR in the explicit "keys()" argument-list form
DROP TABLE IF EXISTS titer15b;
CREATE TABLE titer15b
(
	catsys INTEGER ITERATOR "$$catsys^TITER15(keys(""catsys""))",
	id INTEGER ITERATOR "$$id^TITER15(keys(""catsys""),keys(""id""))",
	iterctx VARCHAR(100) EXTRACT "$$seen^TITER15($$tableName^%ydboctoplanhelpers())",
	extractctx VARCHAR(100) EXTRACT "$$whoami^TITER15",
	PRIMARY KEY (catsys,id)
)
GLOBAL "^TITER15G(keys(""catsys""),keys(""id""))";
SELECT * FROM titer15b;
