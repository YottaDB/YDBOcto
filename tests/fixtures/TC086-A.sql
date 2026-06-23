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
-- Two-level extraction: ^addr nodes are "name|street^apt^city^state|phone"; the address (the 2nd
-- "|"-piece) is itself "^"-delimited. The same delimiters can be spelled several equivalent ways -- all
-- of the tables below extract identical values; they differ only in how the delimiter is written (and how
-- the stored text definition round-trips it).

-- Literal delimiters ("|","^"):
CREATE TABLE addr (
	id INTEGER PRIMARY KEY,
	name VARCHAR(40) PIECE 1,
	street VARCHAR(40) DELIMS ("|","^") PIECES (2,1),
	apt VARCHAR(40) DELIMS ("|","^") PIECES (2,2),
	city VARCHAR(40) DELIMS ("|","^") PIECES (2,3),
	state VARCHAR(40) DELIMS ("|","^") PIECES (2,4),
	phone VARCHAR(40) PIECE 3
) GLOBAL "^addr(keys(""id""))";
\d addr;
SELECT * FROM addr ORDER BY id;

-- Single-quoted delimiters behave identically to the double-quoted form:
CREATE TABLE addr_squote (id INTEGER PRIMARY KEY, street VARCHAR(40) DELIMS ('|','^') PIECES (2,1)) GLOBAL "^addr(keys(""id""))";
\d addr_squote;
SELECT * FROM addr_squote ORDER BY id;

-- $C(124) is "|": a char-code delimiter. The stored text definition round-trips it as $CHAR(124):
CREATE TABLE addr_char (id INTEGER PRIMARY KEY, street VARCHAR(40) DELIMS ($C(124),"^") PIECES (2,1)) GLOBAL "^addr(keys(""id""))";
\d addr_char;
SELECT * FROM addr_char ORDER BY id;

-- Multi-character delimiters ("||","~~") on ^addrm ("name||street~~apt~~city~~state||phone"):
CREATE TABLE addr_multichar (id INTEGER PRIMARY KEY, street VARCHAR(40) DELIMS ("||","~~") PIECES (2,1), city VARCHAR(40) DELIMS ("||","~~") PIECES (2,3)) GLOBAL "^addrm(keys(""id""))";
\d addr_multichar;
SELECT * FROM addr_multichar ORDER BY id;

-- $CHAR(124,124) is the two-character delimiter "||", so this is equivalent to addr_multichar's street:
CREATE TABLE addr_charmulti (id INTEGER PRIMARY KEY, street VARCHAR(40) DELIMS ($CHAR(124,124),"~~") PIECES (2,1)) GLOBAL "^addrm(keys(""id""))";
\d addr_charmulti;
SELECT * FROM addr_charmulti ORDER BY id;
