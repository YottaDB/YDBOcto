#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TC037 : OCTO589 : DELIM "" at the column level invalidates any PIECE specified and fetches entire node

-- Test DELIM "" without PIECE specified at the column level
CREATE TABLE names1 (
	id INTEGER PRIMARY KEY,
	firstName VARCHAR(30) DELIM "",
	lastName TEXT(30)
) GLOBAL "^names(keys(""id""))";

SELECT * FROM names1;

-- Test DELIM "" with PIECE specified at the column level
CREATE TABLE names2 (
	id INTEGER PRIMARY KEY,
	firstName VARCHAR(30) DELIM "" PIECE 2,
	lastName TEXT(30)
) GLOBAL "^names(keys(""id""))";

SELECT * FROM names2;

