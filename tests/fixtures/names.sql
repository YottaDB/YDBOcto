#################################################################
#								#
# Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

CREATE TABLE names (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))";

CREATE TABLE namesWithAges (
 id INTEGER PRIMARY KEY,
 firstName VARCHAR(30),
 lastName VARCHAR(30),
 age INTEGER,
 time VARCHAR(11) EXTRACT "$ZDATE($HOROLOG,""YEAR-MM-DD 24:60:SS"")"	-- this is a computed column
)
 GLOBAL "^names(keys(""id""))";
