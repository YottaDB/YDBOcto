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
-- Error cases. Each CREATE TABLE below is expected to fail with the noted error.

-- 2 DELIMS values but only 1 PIECES value -> ERR_PIECE_DELIM_COUNT_MISMATCH:
CREATE TABLE err_count_delims (id INTEGER PRIMARY KEY, street VARCHAR(40) DELIMS ("|","^") PIECES (2)) GLOBAL "^addr(keys(""id""))";

-- 1 DELIMS value but 2 PIECES values -> ERR_PIECE_DELIM_COUNT_MISMATCH:
CREATE TABLE err_count_pieces (id INTEGER PRIMARY KEY, street VARCHAR(40) DELIMS ("|") PIECES (2,1)) GLOBAL "^addr(keys(""id""))";

-- A non-$CHAR intrinsic inside DELIMS -> ERR_DELIMS_INVALID_INTRINSIC:
CREATE TABLE err_intrinsic (id INTEGER PRIMARY KEY, street VARCHAR(40) DELIMS ($EXTRACT(1),"^") PIECES (2,1)) GLOBAL "^addr(keys(""id""))";

-- Concatenation (or any non-$CHAR M expression) inside DELIMS -> ERR_PARSE_FAILED:
CREATE TABLE err_concat (id INTEGER PRIMARY KEY, street VARCHAR(40) DELIMS ($C(124)_"|","^") PIECES (2,1)) GLOBAL "^addr(keys(""id""))";

-- A piece-of-piece column forces READONLY, so an explicit READWRITE is rejected -> ERR_READWRITE_DISALLOWED:
CREATE TABLE err_readwrite (id INTEGER PRIMARY KEY, street VARCHAR(40) DELIMS ("|","^") PIECES (2,1)) GLOBAL "^addr(keys(""id""))" READWRITE;

-- A multi-element DELIMS (..) is only valid at the column level, not the table level -> ERR_DELIMS_TABLE_LEVEL:
CREATE TABLE err_table_delims (id INTEGER PRIMARY KEY, street VARCHAR(40) PIECE 2) GLOBAL "^addr(keys(""id""))" DELIMS ("^","~");

-- A bare (unquoted) number as a DELIMS delimiter -> ERR_DELIMS_NUMERIC. A DELIMS delimiter must be a quoted
-- string or a $C(..) intrinsic; the number must not be silently coerced to a string (unlike the older single
-- DELIM form). Rejected as the first element, as a later element, on its own, and for a non-integer number:
CREATE TABLE err_delims_int1 (id INTEGER PRIMARY KEY, street VARCHAR(40) DELIMS (1,"^") PIECES (2,1)) GLOBAL "^addr(keys(""id""))";
CREATE TABLE err_delims_int2 (id INTEGER PRIMARY KEY, street VARCHAR(40) DELIMS ("|",2) PIECES (2,1)) GLOBAL "^addr(keys(""id""))";
CREATE TABLE err_delims_int3 (id INTEGER PRIMARY KEY, street VARCHAR(40) DELIMS (5) PIECES (1)) GLOBAL "^addr(keys(""id""))";
CREATE TABLE err_delims_num  (id INTEGER PRIMARY KEY, street VARCHAR(40) DELIMS (1.5,"^") PIECES (2,1)) GLOBAL "^addr(keys(""id""))";

-- A negative number is also rejected, but earlier and via a different path: "-5" lexes as a MINUS token
-- followed by "5", and a DELIMS element cannot start with MINUS, so it fails to parse (ERR_PARSE_FAILED,
-- "unexpected MINUS") before reaching the ERR_DELIMS_NUMERIC check. Either way it is rejected:
CREATE TABLE err_delims_neg1 (id INTEGER PRIMARY KEY, street VARCHAR(40) DELIMS (-5,"^") PIECES (2,1)) GLOBAL "^addr(keys(""id""))";
CREATE TABLE err_delims_neg2 (id INTEGER PRIMARY KEY, street VARCHAR(40) DELIMS (-5) PIECES (1)) GLOBAL "^addr(keys(""id""))";
