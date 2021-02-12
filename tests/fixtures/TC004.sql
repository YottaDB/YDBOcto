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

-- TC004 : recreate a cached table and check that the cached version is deleted

-- Need READONLY below as we do not want the DROP TABLE to delete ^T1 as the following queries rely on ^T1 persisting.

CREATE TABLE t1 (a int primary key) GLOBAL "^T1(keys(""a""))" READONLY;
SELECT * FROM t1;
DROP TABLE t1;
CREATE TABLE t1 (a int primary key, b int) GLOBAL "^T1(keys(""a""))" READONLY;
SELECT * FROM t1;
DROP TABLE t1;
CREATE TABLE t1 (a int primary key, b int, c int) GLOBAL "^T1(keys(""a""))" READONLY;
SELECT * FROM t1;
DROP TABLE t1;
CREATE TABLE t1 (a int primary key, b int, c int, d int) GLOBAL "^T1(keys(""a""))" READONLY;
SELECT * FROM t1;
DROP TABLE t1;

