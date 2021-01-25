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

DROP TABLE t1;
CREATE TABLE t1 (a int primary key);
SELECT * FROM t1;
DROP TABLE t1;
CREATE TABLE t1 (a int primary key, b int);
SELECT * FROM t1;
DROP TABLE t1;
CREATE TABLE t1 (a int primary key, b int, c int);
SELECT * FROM t1;
DROP TABLE t1;
CREATE TABLE t1 (a int primary key, b int, c int, d int);
SELECT * FROM t1;

