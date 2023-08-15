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
-- TITER14 : VIRTUAL is only valid as a suffix to ITERATOR. The grammar enforces the pairing
-- (see "ITERATOR ddl_str_literal_value VIRTUAL ..." in src/parser.y); VIRTUAL appearing
-- anywhere else is a syntax error.
-- INVALID: VIRTUAL on a column with no ITERATOR -> syntax error
DROP TABLE IF EXISTS bad_virtual1;
CREATE TABLE bad_virtual1 (id INTEGER PRIMARY KEY VIRTUAL) GLOBAL "^names";
-- INVALID: VIRTUAL before the ITERATOR string -> syntax error (VIRTUAL is a strict suffix)
DROP TABLE IF EXISTS bad_virtual2;
CREATE TABLE bad_virtual2 (id INTEGER PRIMARY KEY VIRTUAL ITERATOR "$$id^TITER01");
-- VALID: VIRTUAL immediately after the ITERATOR string
DROP TABLE IF EXISTS good_virtual;
CREATE TABLE good_virtual (id INTEGER PRIMARY KEY ITERATOR "$$id^TITER01" VIRTUAL);
-- VALID: "virtual" remains usable as a SQL identifier (table and column name)
DROP TABLE IF EXISTS virtual;
CREATE TABLE virtual (virtual INTEGER PRIMARY KEY) GLOBAL "^names";
