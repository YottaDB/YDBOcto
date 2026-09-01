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

-- TC089 : OCTO1146 : Section C : one label per distinct EXTRACT column, shared by repeat references

CREATE TABLE tc089c (
	id INTEGER PRIMARY KEY,
	firstname VARCHAR(30),
	lastname VARCHAR(30),
	alpha VARCHAR EXTRACT "$$col^TC089(values(""firstname""))",
	beta VARCHAR EXTRACT "$$col^TC089(values(""lastname""))",
	alphatwin VARCHAR EXTRACT "$$col^TC089(values(""firstname""))"
) GLOBAL "^names(keys(""id""))" READONLY;

-- Two EXTRACT columns of one table, each reporting its own name and not the other's.
SELECT id, alpha, beta FROM tc089c WHERE id < 2;

-- The same EXTRACT column referenced twice in one query collapses to a single label.
SELECT alpha, alpha FROM tc089c WHERE id = 0;

-- Two columns with identical EXTRACT text emit identical M, but still get a label each.
SELECT alpha, alphatwin FROM tc089c WHERE id = 0;

-- One column reached through two aliases emits two different bodies (each reads its own alias's key).
SELECT a.alpha, b.alpha FROM tc089c a, tc089c b WHERE a.id = 0 AND b.id = 1;
