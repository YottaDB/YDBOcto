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
-- TC087 : OCTO763 : Whole-node SUBSTR: with no PIECE/PIECES, the character range applies to an entire
-- global node. A column-level GLOBAL is optional: when present it points SUBSTR at that node (here the
-- address lives on its own ^subwn(id,0) node); when absent SUBSTR reads the table's own node ^subwn(id)
-- ("name|phone"), the same node a PIECE column reads. SUBSTR forces the table READONLY (no explicit
-- READONLY keyword below).

CREATE TABLE subwn (
	id INTEGER PRIMARY KEY,
	name VARCHAR PIECE 1,
	address VARCHAR SUBSTR 1-12 GLOBAL "^subwn(keys(""id""),0)",
	initial VARCHAR SUBSTR 1 GLOBAL "^subwn(keys(""id""),0)"
) GLOBAL "^subwn(keys(""id""))" AIMTYPE 1;
-- \d shows Type = READONLY (forced by SUBSTR):
\d subwn;
SELECT * FROM subwn ORDER BY id;

-- Round-trip spelling: the stored text definition appends the implicit DELIM "" AFTER the GLOBAL
-- (keywords after an explicit DELIM "" are dropped by the parser, so the order below is the one
-- the text definition emits). This table must behave identically to subwn's address column:
CREATE TABLE subwn_rt (
	id INTEGER PRIMARY KEY,
	address VARCHAR SUBSTR 1-12 GLOBAL "^subwn(keys(""id""),0)" DELIM ""
) GLOBAL "^subwn(keys(""id""))" AIMTYPE 1;
\d subwn_rt;
SELECT * FROM subwn_rt ORDER BY id;

-- Whole-node SUBSTR with NO column-level GLOBAL: the range reads the table's own node ^subwn(id)
-- ("name|phone"), not the ^subwn(id,0) subnode subwn reads above. A column-level GLOBAL is optional
-- for the whole-node form. \d shows the appended DELIM "".
CREATE TABLE subwn_tn (
	id INTEGER PRIMARY KEY,
	head9 VARCHAR SUBSTR 1-9
) GLOBAL "^subwn(keys(""id""))";
\d subwn_tn;
SELECT * FROM subwn_tn ORDER BY id;
-- The no-GLOBAL whole-node form is cross-referenced like any other SUBSTR column: a WHERE on it builds
-- and uses an AIM index (rather than a full scan) just like the column-GLOBAL form does.
SELECT id FROM subwn_tn WHERE head9 = 'Acid Burn' ORDER BY id;
