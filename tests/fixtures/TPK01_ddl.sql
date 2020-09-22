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

-- TPK01 : OCTO279 : Octo returns incorrect results if the key column is the empty string

-- All columns are key columns
CREATE TABLE T1(a INT STARTINCLUDE, b INT STARTINCLUDE, c INT STARTINCLUDE, d INT STARTINCLUDE, e INT STARTINCLUDE);

-- Some columns are not key columns
CREATE TABLE T2(a VARCHAR(10) PRIMARY KEY STARTINCLUDE, b VARCHAR(10) KEY NUM 1 STARTINCLUDE, c VARCHAR(10) STARTINCLUDE);

-- Test that when START is specified, we go PAST the START value and not include it in the FOR loop
-- For this, we will use vista-mini.sql and vista-mini.zwr that are already available. No separate CREATE TABLE needed.

