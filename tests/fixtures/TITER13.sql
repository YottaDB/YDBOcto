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
-- TITER13 : ITERATOR accepts an explicit argument list with keys() substitution
-- Case 1 (positive) : bare entryref form, Octo auto-appends keys (no parens)
DROP TABLE IF EXISTS t13a;
CREATE TABLE t13a
(
	catsys VARCHAR(30) ITERATOR "$$catsys^TITER01",
	id integer ITERATOR "$$id^TITER01",
	idx integer ITERATOR "$$idx^TITER01",
	title VARCHAR(200) EXTRACT "$get(RAti(keys(""idx""),""ti""))",
	PRIMARY KEY (catsys,id,idx)
)
DELIM "^"
GLOBAL "^BCAT(keys(""catsys""),keys(""id""),""title"",keys(""idx""))";
SELECT * FROM t13a;
-- Case 2 (positive) : explicit keys() form using the same M routines
DROP TABLE IF EXISTS t13b;
CREATE TABLE t13b
(
	catsys VARCHAR(30) ITERATOR "$$catsys^TITER01(keys(""catsys""))",
	id integer ITERATOR "$$id^TITER01(keys(""catsys""),keys(""id""))",
	idx integer ITERATOR "$$idx^TITER01(keys(""catsys""),keys(""id""),keys(""idx""))",
	title VARCHAR(200) EXTRACT "$get(RAti(keys(""idx""),""ti""))",
	PRIMARY KEY (catsys,id,idx)
)
DELIM "^"
GLOBAL "^BCAT(keys(""catsys""),keys(""id""),""title"",keys(""idx""))";
SELECT * FROM t13b;
-- Equivalence : explicit-form (t13b) and bare-entryref form (t13a) must return identical rows.
-- Both EXCEPT queries should return zero rows.
SELECT * FROM t13a EXCEPT SELECT * FROM t13b;
SELECT * FROM t13b EXCEPT SELECT * FROM t13a;
-- Case 3 (positive) : explicit keys() plus a literal extra argument
DROP TABLE IF EXISTS t13c;
CREATE TABLE t13c
(
	catsys VARCHAR(30) ITERATOR "$$catsys^TITER13(""mylabel"",keys(""catsys""))",
	id integer ITERATOR "$$id^TITER13(""mylabel"",keys(""catsys""),keys(""id""))",
	idx integer ITERATOR "$$idx^TITER13(""mylabel"",keys(""catsys""),keys(""id""),keys(""idx""))",
	title VARCHAR(200) EXTRACT "$get(RAti(keys(""idx""),""ti""))",
	PRIMARY KEY (catsys,id,idx)
)
DELIM "^"
GLOBAL "^BCAT(keys(""catsys""),keys(""id""),""title"",keys(""idx""))";
SELECT * FROM t13c;
-- Case 4 (rejected) : empty argument list "()" is disallowed -- the bare entryref form
-- should be used instead when the user wants Octo to auto-append keys.
CREATE TABLE t13d
(
	catsys VARCHAR(30) ITERATOR "$$catsys^TITER01()",
	id integer,
	idx integer,
	PRIMARY KEY (catsys,id,idx)
)
DELIM "^"
GLOBAL "^BCAT(keys(""catsys""),keys(""id""),keys(""idx""))";
-- Case 5 (rejected) : keys() references a column that does not exist
CREATE TABLE t13e
(
	catsys VARCHAR(30) ITERATOR "$$catsys^TITER01(keys(""no_such_col""))",
	id integer,
	idx integer,
	PRIMARY KEY (catsys,id,idx)
)
DELIM "^"
GLOBAL "^BCAT(keys(""catsys""),keys(""id""),keys(""idx""))";
-- Case 6 (rejected) : keys() references a non-KEY column
CREATE TABLE t13f
(
	catsys VARCHAR(30) ITERATOR "$$catsys^TITER01(keys(""title""))",
	id integer,
	idx integer,
	title VARCHAR(200) EXTRACT "$get(RAti(keys(""idx""),""ti""))",
	PRIMARY KEY (catsys,id,idx)
)
DELIM "^"
GLOBAL "^BCAT(keys(""catsys""),keys(""id""),keys(""idx""))";
-- Case 7 (rejected) : values() is not allowed in an ITERATOR argument list
CREATE TABLE t13g
(
	catsys VARCHAR(30) ITERATOR "$$catsys^TITER01(values(""title""))",
	id integer,
	idx integer,
	title VARCHAR(200) EXTRACT "$get(RAti(keys(""idx""),""ti""))",
	PRIMARY KEY (catsys,id,idx)
)
DELIM "^"
GLOBAL "^BCAT(keys(""catsys""),keys(""id""),keys(""idx""))";
-- Case 8 (rejected) : ITERATOR value starts with '(' (no entryref prefix)
CREATE TABLE t13h
(
	catsys VARCHAR(30) ITERATOR "(keys(""catsys""))",
	id integer,
	idx integer,
	PRIMARY KEY (catsys,id,idx)
)
DELIM "^"
GLOBAL "^BCAT(keys(""catsys""),keys(""id""),keys(""idx""))";
-- Case 9 (rejected) : trailing junk after the closing ')'
CREATE TABLE t13i
(
	catsys VARCHAR(30) ITERATOR "$$catsys^TITER01(keys(""catsys""))trailing",
	id integer,
	idx integer,
	PRIMARY KEY (catsys,id,idx)
)
DELIM "^"
GLOBAL "^BCAT(keys(""catsys""),keys(""id""),keys(""idx""))";
-- Case 10 (positive) : keys() may reference a KEY column that does NOT itself carry the
-- ITERATOR keyword. The keys() check only requires the referenced column to be a KEY
-- column; whether it is iterated by Octo (no ITERATOR) or by the user (ITERATOR) is
-- irrelevant here. "id" has no ITERATOR but is still a valid keys() target in the
-- arglist of "idx"'s iterator.
DROP TABLE IF EXISTS t13j;
CREATE TABLE t13j
(
	catsys VARCHAR(30),
	id integer,
	idx integer ITERATOR "$$idx^TITER01(keys(""catsys""),keys(""id""),keys(""idx""))",
	title VARCHAR(200) EXTRACT "$get(RAti(keys(""idx""),""ti""))",
	PRIMARY KEY (catsys,id,idx)
)
DELIM "^"
GLOBAL "^BCAT(keys(""catsys""),keys(""id""),""title"")";
-- Case 11 (rejected) : keys("title") appears before keys("idx") in the arglist. "title"
-- is NOT a KEY column, so the keys-must-be-KEY-column check fires regardless of where
-- "title" sits relative to other keys() tokens.
CREATE TABLE t13k
(
	catsys VARCHAR(30),
	id integer,
	idx integer ITERATOR "$$idx^TITER01(keys(""catsys""),keys(""id""),keys(""title""),keys(""idx""))",
	title VARCHAR(200) EXTRACT "$get(RAti(keys(""idx""),""ti""))",
	PRIMARY KEY (catsys,id,idx)
)
DELIM "^"
GLOBAL "^BCAT(keys(""catsys""),keys(""id""),keys(""idx""))";
-- Case 12 (positive) : only the MIDDLE key column ("id") has an ITERATOR. The first
-- ("catsys") and last ("idx") key columns iterate via the default $ORDER walk of the
-- data global. "id"'s explicit arglist references both KEY columns it depends on.
DROP TABLE IF EXISTS t13m;
CREATE TABLE t13m
(
	catsys VARCHAR(30),
	id integer ITERATOR "$$id^TITER01(keys(""catsys""),keys(""id""))",
	idx integer,
	title VARCHAR(200) EXTRACT "$get(RAti(keys(""idx""),""ti""))",
	PRIMARY KEY (catsys,id,idx)
)
DELIM "^"
GLOBAL "^BCAT(keys(""catsys""),""title"",keys(""idx""))";
