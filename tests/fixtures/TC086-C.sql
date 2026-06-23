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
-- A single-element DELIMS (..) / PIECES (..) is collapsed (in the parser) to the plain DELIM / PIECE form,
-- so it is NOT a piece-of-piece column: a single (non-nested) $PIECE is generated, and the column counts
-- toward the default piece number exactly like an explicit PIECE. downgrade_col (written with DELIMS/PIECES)
-- and downgrade_col_plain (written with DELIM/PIECE) therefore produce byte-identical stored definitions.
-- The trailing "extra" column has no PIECE and follows explicit-piece columns, so it exercises the default
-- piece-counting path (it gets the same piece number in both). Both are READONLY because the first non-key
-- column has an explicit DELIM -- an existing rule that applies the same way to DELIM and to DELIMS.
CREATE TABLE downgrade_col (
	id INTEGER PRIMARY KEY,
	fn VARCHAR(40) DELIMS ("|") PIECES (1),
	ln VARCHAR(40) DELIMS ("|") PIECES (2),
	extra VARCHAR(40)
) GLOBAL "^dgrade(keys(""id""))";
\d downgrade_col;
SELECT * FROM downgrade_col ORDER BY id;

CREATE TABLE downgrade_col_plain (
	id INTEGER PRIMARY KEY,
	fn VARCHAR(40) DELIM "|" PIECE 1,
	ln VARCHAR(40) DELIM "|" PIECE 2,
	extra VARCHAR(40)
) GLOBAL "^dgrade(keys(""id""))";
\d downgrade_col_plain;
SELECT * FROM downgrade_col_plain ORDER BY id;

-- The downgrade also applies at the TABLE level: a single-element table-level DELIMS ("^") is accepted and
-- is equivalent to DELIM "^". downgrade_tbl (DELIMS) and downgrade_tbl_plain (DELIM) round-trip identically.
CREATE TABLE downgrade_tbl (id INTEGER PRIMARY KEY, fn VARCHAR(40), ln VARCHAR(40)) GLOBAL "^dgradet(keys(""id""))" DELIMS ("^");
\d downgrade_tbl;
SELECT * FROM downgrade_tbl ORDER BY id;

CREATE TABLE downgrade_tbl_plain (id INTEGER PRIMARY KEY, fn VARCHAR(40), ln VARCHAR(40)) GLOBAL "^dgradet(keys(""id""))" DELIM "^";
\d downgrade_tbl_plain;
SELECT * FROM downgrade_tbl_plain ORDER BY id;

-- An empty delimiter is a pre-existing special case: DELIM "" invalidates any PIECE and fetches the ENTIRE
-- node (no $PIECE is generated). A single-element DELIMS ("") collapses (in the parser) to that same empty
-- DELIM "" form, so it too drops the PIECE and fetches the whole node -- it never reaches the multi-element
-- (piece-of-piece) code path. downgrade_empty (DELIMS ("")) and downgrade_empty_plain (DELIM "") therefore
-- extract the whole "fn|ln" node identically and round-trip to byte-identical stored definitions.
CREATE TABLE downgrade_empty (id INTEGER PRIMARY KEY, whole VARCHAR(40) DELIMS ("") PIECES (2)) GLOBAL "^dgrade(keys(""id""))";
\d downgrade_empty;
SELECT * FROM downgrade_empty ORDER BY id;

CREATE TABLE downgrade_empty_plain (id INTEGER PRIMARY KEY, whole VARCHAR(40) DELIM "" PIECE 2) GLOBAL "^dgrade(keys(""id""))";
\d downgrade_empty_plain;
SELECT * FROM downgrade_empty_plain ORDER BY id;
