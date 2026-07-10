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
-- TC087 : OCTO763 : SUBSTR composes with column types, DELIMS/PIECES, values() references and LEFT JOIN.

-- Typed columns: the range is applied to the piece text BEFORE the type conversion.
-- ^subtyp nodes are "<date+junk>|<flagword>|QTYnn":
CREATE TABLE subtyp (
	id INTEGER PRIMARY KEY,
	dob DATE PIECE 1 SUBSTR 1-10,
	flag BOOLEAN PIECE 2 SUBSTR 2-4,
	qty INTEGER PIECE 3 SUBSTR 4-5
) GLOBAL "^subtyp(keys(""id""))";
SELECT * FROM subtyp ORDER BY id;

-- Malformed strongly-typed data reached by SUBSTR: SUBSTR slices the piece text BEFORE the type
-- conversion, exactly like a plain PIECE column on a READONLY table. When the sliced text is not a
-- valid value of the column type, Octo handles it gracefully rather than erroring: INTEGER/NUMERIC
-- pass the raw sliced text through, BOOLEAN forces to FALSE, and an invalid DATE reads as SQL NULL.
-- Slicing can even RESCUE a value by cutting off trailing junk (id 2: "42abc" -> "42" -> 42).
-- ^subtypx nodes are "<int>|<numeric>|<bool>|<date+junk>":
--   0: valid after slicing ("2020-01-15abc" -> "2020-01-15")
--   1: still malformed after slicing ("9X" int, "1.2." numeric, "2" bool, "2020-13-45" bad date)
--   2: int/numeric rescued by slicing off trailing junk
-- "flag_p" is a plain PIECE 3 BOOLEAN reading the SAME single-character piece as the SUBSTR column
-- "flag" (piece 3 is one char, so SUBSTR 1-1 and the whole piece are identical bytes). Their output is
-- identical -- both force the malformed "2" to FALSE -- confirming SUBSTR reuses the same piece->column
-- conversion PIECE does. (Whether an unrecognized BOOLEAN becomes FALSE or NULL is a READONLY-vs-
-- READWRITE distinction, not a SUBSTR-vs-PIECE one; SUBSTR forces READONLY, so it always matches a
-- plain PIECE column on a READONLY table.)
-- "inum_p" is likewise a plain PIECE 1 INTEGER. On a READONLY table Octo does NOT validate INTEGER
-- (or NUMERIC) data: non-numeric text is displayed raw (id 1 "9X99", id 2 "42abc"), for PIECE and
-- SUBSTR alike. "inum" (SUBSTR 1-2) differs from "inum_p" only because it slices piece 1 to 2 chars
-- first -- which is how the slice trims the trailing junk off id 2 ("42abc" -> "42") while id 1 stays
-- malformed ("9X99" -> "9X").
CREATE TABLE subtypx (
	id     INTEGER PRIMARY KEY,
	inum   INTEGER PIECE 1 SUBSTR 1-2,
	inum_p INTEGER PIECE 1,
	dec    NUMERIC PIECE 2 SUBSTR 1-4,
	flag   BOOLEAN PIECE 3 SUBSTR 1-1,
	flag_p BOOLEAN PIECE 3,
	dt     DATE    PIECE 4 SUBSTR 1-10
) GLOBAL "^subtypx(keys(""id""))";
SELECT * FROM subtypx ORDER BY id;
-- WHERE / IS NULL over the malformed-typed columns must not error:
SELECT id FROM subtypx WHERE inum = 42 ORDER BY id;
SELECT id FROM subtypx WHERE flag = FALSE ORDER BY id;
SELECT id FROM subtypx WHERE dt IS NULL ORDER BY id;

-- DELIMS/PIECES + SUBSTR: the range applies to the innermost extracted piece
-- ("A|45 Oak^Boston|z" -> "|"-piece 2 -> "^"-piece 2 -> chars 1-3):
CREATE TABLE subpp (
	id INTEGER PRIMARY KEY,
	city3 VARCHAR DELIMS ("|","^") PIECES (2,2) SUBSTR 1-3
) GLOBAL "^subpp(keys(""id""))";
\d subpp;
SELECT * FROM subpp ORDER BY id;

-- values() re-derivation: an EXTRACT column referencing a whole-node SUBSTR column re-emits the
-- $EXTRACT($GET(...)) (and must NOT re-slice it into $PIECEs):
CREATE TABLE subval (
	id INTEGER PRIMARY KEY,
	address VARCHAR SUBSTR 1-12 GLOBAL "^subval(keys(""id""),0)",
	addr_upper VARCHAR EXTRACT "$ZCONVERT(values(""address""),""U"")"
) GLOBAL "^subval(keys(""id""))" AIMTYPE 1;
SELECT * FROM subval ORDER BY id;

-- values() of a PIECE + SUBSTR column:
CREATE TABLE subval2 (
	id INTEGER PRIMARY KEY,
	birthyear INTEGER PIECE 2 SUBSTR 1-4,
	century VARCHAR EXTRACT "$EXTRACT(values(""birthyear""),1,2)"
) GLOBAL "^subval2(keys(""id""))";
SELECT * FROM subval2 ORDER BY id;

-- LEFT JOIN: the unmatched row (id 9) must come back NULL, not error ($EXTRACT of $ZYSQLNULL):
SELECT n.id, s.address FROM (SELECT 0 AS id UNION SELECT 9 AS id) n LEFT JOIN subval s ON n.id = s.id ORDER BY n.id;

-- Keyless READONLY table: with no user-declared key column, the auto-key pass promotes every visible
-- column to a key column -- EXCEPT a SUBSTR column, which is derived from the node/piece value and so
-- cannot be a key (same exclusion EXTRACT columns get). Here "who" becomes the key and the SUBSTR
-- column "area" stays a non-key derived column. Without the SUBSTR exclusion, "area" would be made a
-- key and the CREATE would fail with ERR_SUBSTR_CANNOT_BE_KEY_COLUMN.
CREATE TABLE subkl (
	who VARCHAR PIECE 1,
	area VARCHAR PIECE 2 SUBSTR 1-3
) GLOBAL "^subkl(keys(""who""))";
\d subkl;
SELECT * FROM subkl ORDER BY who;
