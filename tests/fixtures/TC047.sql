
#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TC047 : OCTO672 : GLOBAL keyword in CREATE TABLE command should accept just an unsubscripted global name

SELECT '-- Simple test case first';
CREATE TABLE tmp1 (id INTEGER PRIMARY KEY, firstName VARCHAR, lastName TEXT) GLOBAL "^names";
SELECT * from tmp1;

SELECT '-- Test for READONLY with user specified primary key columns';
CREATE TABLE tmp2 (id INTEGER PRIMARY KEY, firstName VARCHAR, lastName TEXT) GLOBAL "^names" READONLY;
SELECT * from tmp2;

SELECT '-- Test for READONLY with no user specified primary key columns';
CREATE TABLE tmp3 (id INTEGER) GLOBAL "^names" READONLY;
SELECT * from tmp3;

SELECT '-- Test for READWRITE with user specified primary key columns';
CREATE TABLE tmp4 (id INTEGER PRIMARY KEY, firstName VARCHAR, lastName TEXT) GLOBAL "^names" READWRITE;
SELECT * from tmp4;

SELECT '-- Test for READWRITE with no user specified primary key columns';
CREATE TABLE tmp5 (firstandlastname VARCHAR) GLOBAL "^names" READWRITE;
SELECT * from tmp5;

