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
-- The AIM transform composes the piece-of-piece extraction with the column's OWN conversion. Each
-- conversion is shown at 2 levels ("x|<v>^<extra>|z") and 3 levels ("a|b^c~<v>~e^f|g").

-- DATE -> Transform2UnixTime. Expect ids 0 and 2 for 2020-01-15.
CREATE TABLE ppdt (id INTEGER PRIMARY KEY, dob DATE DELIMS ("|","^") PIECES (2,1)) GLOBAL "^ppdt(keys(""id""))";
SELECT * FROM ppdt ORDER BY id;
SELECT * FROM ppdt WHERE dob = date'2020-01-15' ORDER BY id;
CREATE TABLE ppdt3 (id INTEGER PRIMARY KEY, dob DATE DELIMS ("|","^","~") PIECES (2,2,2)) GLOBAL "^ppdt3(keys(""id""))";
SELECT * FROM ppdt3 ORDER BY id;
SELECT * FROM ppdt3 WHERE dob = date'2020-01-15' ORDER BY id;

-- BOOLEAN -> ForceBoolean. Expect ids 0 and 2 for TRUE.
CREATE TABLE ppflag (id INTEGER PRIMARY KEY, flag BOOLEAN DELIMS ("|","^") PIECES (2,1)) GLOBAL "^ppflag(keys(""id""))";
SELECT * FROM ppflag ORDER BY id;
SELECT * FROM ppflag WHERE flag = TRUE ORDER BY id;
CREATE TABLE ppflag3 (id INTEGER PRIMARY KEY, flag BOOLEAN DELIMS ("|","^","~") PIECES (2,2,2)) GLOBAL "^ppflag3(keys(""id""))";
SELECT * FROM ppflag3 ORDER BY id;
SELECT * FROM ppflag3 WHERE flag = TRUE ORDER BY id;

-- INTEGER -> no conversion (numeric, not lexical, collation). MIN/MAX give 5/100, not the lexical 100/7.
CREATE TABLE ppqty (id INTEGER PRIMARY KEY, qty INTEGER DELIMS ("|","^") PIECES (2,1)) GLOBAL "^ppqty(keys(""id""))";
SELECT * FROM ppqty ORDER BY id;
SELECT * FROM ppqty WHERE qty = 100 ORDER BY id;
SELECT MIN(qty) AS lo, MAX(qty) AS hi FROM ppqty;
