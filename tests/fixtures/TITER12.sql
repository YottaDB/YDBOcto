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
-- TITER12 : ITERATOR key columns may be omitted from GLOBAL keys()
-- Case 1 (allowed) : trailing ITERATOR key column omitted from GLOBAL
DROP TABLE IF EXISTS t12a;
CREATE TABLE t12a
(
	catsys VARCHAR(30),
	id integer,
	idx integer ITERATOR "$$idx^TITER01",
	title VARCHAR(200) EXTRACT "$get(RAti(keys(""idx""),""ti""))",
	PRIMARY KEY (catsys,id,idx)
)
DELIM "^"
GLOBAL "^BCAT(keys(""catsys""),keys(""id""),""title"")";
-- Case 2 (allowed) : middle ITERATOR key column omitted from GLOBAL
DROP TABLE IF EXISTS t12b;
CREATE TABLE t12b
(
	catsys VARCHAR(30),
	id integer ITERATOR "$$id^TITER01",
	idx integer,
	title VARCHAR(200) EXTRACT "$get(RAti(keys(""idx""),""ti""))",
	PRIMARY KEY (catsys,id,idx)
)
DELIM "^"
GLOBAL "^BCAT(keys(""catsys""),""title"",keys(""idx""))";
-- Case 3 (allowed) : ITERATOR key explicitly mentioned in GLOBAL is still accepted
DROP TABLE IF EXISTS t12c;
CREATE TABLE t12c
(
	catsys VARCHAR(30),
	id integer ITERATOR "$$id^TITER01",
	idx integer,
	title VARCHAR(200) EXTRACT "$get(RAti(keys(""idx""),""ti""))",
	PRIMARY KEY (catsys,id,idx)
)
DELIM "^"
GLOBAL "^BCAT(keys(""catsys""),keys(""id""),""title"",keys(""idx""))";
-- Case 4 (rejected) : a non-ITERATOR key column may NOT be omitted from GLOBAL
DROP TABLE IF EXISTS t12d;
CREATE TABLE t12d
(
	catsys VARCHAR(30),
	id integer,
	idx integer ITERATOR "$$idx^TITER01",
	title VARCHAR(200) EXTRACT "$get(RAti(keys(""idx""),""ti""))",
	PRIMARY KEY (catsys,id,idx)
)
DELIM "^"
GLOBAL "^BCAT(keys(""catsys""),""title"")";
-- Case 5 (rejected) : non-ITERATOR keys mentioned out of order still error
DROP TABLE IF EXISTS t12e;
CREATE TABLE t12e
(
	catsys VARCHAR(30),
	id integer,
	idx integer ITERATOR "$$idx^TITER01",
	title VARCHAR(200) EXTRACT "$get(RAti(keys(""idx""),""ti""))",
	PRIMARY KEY (catsys,id,idx)
)
DELIM "^"
GLOBAL "^BCAT(keys(""id""),keys(""catsys""),""title"")";
