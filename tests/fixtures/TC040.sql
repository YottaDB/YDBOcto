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

-- TC040 : OCTO590 : Confirm enforcement of table name length limits during table creation and cleanup

-- 64-byte table name (exceeds 63-byte limit and so should fail)
CREATE TABLE namesLongStringABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvw (
        id INTEGER PRIMARY KEY,
        firstName VARCHAR(30),
        lastName TEXT(30)
) GLOBAL "^names(keys(""id""))";

SELECT * from namesLongStringABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvw;

-- 63-byte table name (meets 63-byte limit and so should succeed)
CREATE TABLE namesLongStringABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuv (
        id INTEGER PRIMARY KEY,
        firstName VARCHAR(30),
        lastName TEXT(30)
) GLOBAL "^names(keys(""id""))";

SELECT * from namesLongStringABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuv;

-- 64-byte table name using '.' to force 'LITERAL PERIOD LITERAL' parser code path (exceeds 63-byte limit and so should fail)
CREATE TABLE namesLongString.ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvw (
        id INTEGER PRIMARY KEY,
        firstName VARCHAR(30),
        lastName TEXT(30)
) GLOBAL "^names(keys(""id""))";

SELECT * from namesLongString.ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuv;

