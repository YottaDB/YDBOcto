#################################################################
#								#
# Copyright (c) 2020-2023 YottaDB LLC and/or its subsidiaries.	#
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

-- 63-byte compound table name (meets 63-byte limit and so should succeed)
CREATE TABLE x.namesLongStringABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrst (
        id INTEGER PRIMARY KEY,
        firstName VARCHAR(30),
        lastName TEXT(30)
) GLOBAL "^names(keys(""id""))";

SELECT * from x.namesLongStringABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrst;

-- Compound table name containing 62-byte identifier following '.', should fail after concatenation of both sides of '.'
DROP TABLE IF EXISTS namesLongStringABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuv;
CREATE TABLE x.namesLongStringABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstu (
        id INTEGER PRIMARY KEY,
        firstName VARCHAR(30),
        lastName TEXT(30)
) GLOBAL "^names(keys(""id""))";

SELECT * from x.namesLongStringABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstu;

-- Compound table name containing 62-byte identifier preceding '.', should fail after concatenation of both sides of '.'
DROP TABLE IF EXISTS namesLongStringABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuv;
CREATE TABLE namesLongStringABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstu.x (
        id INTEGER PRIMARY KEY,
        firstName VARCHAR(30),
        lastName TEXT(30)
) GLOBAL "^names(keys(""id""))";

SELECT * from namesLongStringABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstu.x;

-- Compound table name containing 63-byte identifier following '.', should fail after concatenation of both sides of '.'
DROP TABLE IF EXISTS namesLongStringABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuv;
CREATE TABLE x.namesLongStringABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuv (
        id INTEGER PRIMARY KEY,
        firstName VARCHAR(30),
        lastName TEXT(30)
) GLOBAL "^names(keys(""id""))";

SELECT * from x.namesLongStringABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuv;

-- Compound table name containing 63-byte identifier preceding '.', should fail after concatenation of both sides of '.'
DROP TABLE IF EXISTS namesLongStringABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuv;
CREATE TABLE namesLongStringABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuv.x (
        id INTEGER PRIMARY KEY,
        firstName VARCHAR(30),
        lastName TEXT(30)
) GLOBAL "^names(keys(""id""))";

SELECT * from namesLongStringABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuv.x;

-- Compound table name containing 64-byte identifier following '.', should fail before concatenation of both sides of '.'
CREATE TABLE x.namesLongStringABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvw (
        id INTEGER PRIMARY KEY,
        firstName VARCHAR(30),
        lastName TEXT(30)
) GLOBAL "^names(keys(""id""))";

SELECT * from x.namesLongStringABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvw;

-- Compound table name containing 64-byte identifier preceding '.', should fail before concatenation of both sides of '.'
CREATE TABLE namesLongStringABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvw.x (
        id INTEGER PRIMARY KEY,
        firstName VARCHAR(30),
        lastName TEXT(30)
) GLOBAL "^names(keys(""id""))";

SELECT * from x.namesLongStringABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvw;
