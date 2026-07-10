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
-- The SubstrXform transform composes the $EXTRACT with the column's OWN conversion, so the index
-- holds the fully converted value: DATE -> unix time, BOOLEAN -> 0/1, INTEGER -> raw number.

-- DATE (whole-node form): ^sxdt(id,0) is "YYYY-MM-DDThh:mm"; chars 1-10 are the date. Expect ids 0, 2:
CREATE TABLE sxdt (
	id INTEGER PRIMARY KEY,
	dob DATE SUBSTR 1-10 GLOBAL "^sxdt(keys(""id""),0)"
) GLOBAL "^sxdt(keys(""id""))" AIMTYPE 1;
SELECT * FROM sxdt ORDER BY id;
SELECT id, dob FROM sxdt WHERE dob = date'2020-01-15' ORDER BY id;

-- BOOLEAN: ^sxfl(id) is "x|<digit><word>"; chars 2-4 of piece 2 give yes/no. Expect ids 0, 2 for TRUE:
CREATE TABLE sxfl (
	id INTEGER PRIMARY KEY,
	flag BOOLEAN PIECE 2 SUBSTR 2-4
) GLOBAL "^sxfl(keys(""id""))";
SELECT * FROM sxfl ORDER BY id;
SELECT id FROM sxfl WHERE flag = TRUE ORDER BY id;

-- INTEGER: ^sxqty(id) is "x|QTYnn"; chars 4-6 of piece 2 are the quantity (a range past the end of
-- the text, as in "QTY5", just returns the shorter value). Numeric (not lexical) collation:
-- MIN/MAX give 5/100, and WHERE qty = 100 finds id 2:
CREATE TABLE sxqty (
	id INTEGER PRIMARY KEY,
	qty INTEGER PIECE 2 SUBSTR 4-6
) GLOBAL "^sxqty(keys(""id""))";
SELECT * FROM sxqty ORDER BY id;
SELECT id, qty FROM sxqty WHERE qty = 100;
SELECT MIN(qty) AS lo, MAX(qty) AS hi FROM sxqty;

-- AIMTYPE 1 + whole-node SUBSTR, for every data type. Every ^sxnul row has its (id,0) node; id 2
-- has NONE of the four data nodes -- MISSING NODES, indexed as NULL (AIM metadata type 3) under
-- the bare "" subscript when the index is BUILT (the missing-node rows are found by the initial
-- scan, not just by later triggers) -- and id 3 has all four nodes but each extraction comes
-- back empty (a range past the end, or an empty node value for dob). Both flavors are SQL NULL,
-- so every indexed IS NULL lookup returns ids 2 and 3; MIN/MAX skip them:
CREATE TABLE sxnul (
	id INTEGER PRIMARY KEY,
	rowname VARCHAR PIECE 1 GLOBAL "^sxnul(keys(""id""),0)",
	flag BOOLEAN SUBSTR 2-4 GLOBAL "^sxnul(keys(""id""),1)",
	qty INTEGER SUBSTR 4-5 GLOBAL "^sxnul(keys(""id""),2)",
	dob DATE SUBSTR 1-10 GLOBAL "^sxnul(keys(""id""),3)",
	tag VARCHAR SUBSTR 4-6 GLOBAL "^sxnul(keys(""id""),4)"
) GLOBAL "^sxnul(keys(""id""))" AIMTYPE 1;
SELECT * FROM sxnul ORDER BY id;
SELECT id FROM sxnul WHERE flag IS NULL ORDER BY id;
SELECT id FROM sxnul WHERE qty IS NULL ORDER BY id;
SELECT id FROM sxnul WHERE dob IS NULL ORDER BY id;
SELECT id FROM sxnul WHERE tag IS NULL ORDER BY id;
SELECT id FROM sxnul WHERE flag = TRUE ORDER BY id;
SELECT MIN(qty) AS lo, MAX(qty) AS hi FROM sxnul;
