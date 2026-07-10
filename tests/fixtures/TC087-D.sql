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
-- TC087 : OCTO763 : Error cases. Each CREATE TABLE below is expected to fail with the noted error.

-- end < start -> ERR_SUBSTR_INVALID_RANGE:
CREATE TABLE err_range (id INTEGER PRIMARY KEY, v VARCHAR SUBSTR 5-2 GLOBAL "^subwn(keys(""id""),0)") GLOBAL "^subwn(keys(""id""))";

-- start < 1 -> ERR_SUBSTR_INVALID_RANGE:
CREATE TABLE err_zero (id INTEGER PRIMARY KEY, v VARCHAR SUBSTR 0 GLOBAL "^subwn(keys(""id""),0)") GLOBAL "^subwn(keys(""id""))";

-- negative start: the lexer splits "-1" into MINUS and 1, so no integer literal follows SUBSTR -> syntax error:
CREATE TABLE err_neg (id INTEGER PRIMARY KEY, v VARCHAR SUBSTR -1 GLOBAL "^subwn(keys(""id""),0)") GLOBAL "^subwn(keys(""id""))";

-- non-numeric position: "A" is not a literal -> syntax error:
CREATE TABLE err_alpha (id INTEGER PRIMARY KEY, v VARCHAR SUBSTR A GLOBAL "^subwn(keys(""id""),0)") GLOBAL "^subwn(keys(""id""))";

-- non-integer position: "1.5" is a literal but not an integer -> ERR_DDL_LITERAL:
CREATE TABLE err_frac (id INTEGER PRIMARY KEY, v VARCHAR SUBSTR 1.5 GLOBAL "^subwn(keys(""id""),0)") GLOBAL "^subwn(keys(""id""))";

-- SUBSTR on a key column -> ERR_SUBSTR_CANNOT_BE_KEY_COLUMN:
CREATE TABLE err_key (id INTEGER PRIMARY KEY SUBSTR 1-2, v VARCHAR PIECE 1) GLOBAL "^subwn(keys(""id""))";

-- SUBSTR combined with EXTRACT -> ERR_SUBSTR_EXTRACT_INCOMPATIBLE:
CREATE TABLE err_extract (id INTEGER PRIMARY KEY, v VARCHAR SUBSTR 1-2 EXTRACT "$GET(^subwn(keys(""id"")))") GLOBAL "^subwn(keys(""id""))";

-- non-empty DELIM without a PIECE -> ERR_SUBSTR_DELIM_WITHOUT_PIECE:
CREATE TABLE err_delim (id INTEGER PRIMARY KEY, v VARCHAR SUBSTR 1-2 DELIM "^" GLOBAL "^subwn(keys(""id""),0)") GLOBAL "^subwn(keys(""id""))";

-- DELIMS without PIECES is also a non-empty delimiter -> ERR_SUBSTR_DELIM_WITHOUT_PIECE:
CREATE TABLE err_delims (id INTEGER PRIMARY KEY, v VARCHAR SUBSTR 1-2 DELIMS ("|","^") GLOBAL "^subwn(keys(""id""),0)") GLOBAL "^subwn(keys(""id""))";

-- SUBSTR forces READONLY, so an explicit READWRITE is rejected -> ERR_READWRITE_DISALLOWED:
CREATE TABLE err_rw (id INTEGER PRIMARY KEY, v VARCHAR SUBSTR 1-2 GLOBAL "^subwn(keys(""id""),0)") GLOBAL "^subwn(keys(""id""))" READWRITE;
