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
-- Delimiter edge cases in the AIM path.

-- (1) $CHAR delimiter: ^ppcaddr uses $C(124)="|" (inner) and $C(94)="^" (outer). The inner delimiter is
--     passed to XREFDATA verbatim as $CHAR(124); the resulting index is identical to the literal form.
CREATE TABLE ppcaddr (id INTEGER PRIMARY KEY, street VARCHAR DELIMS ($C(124),$C(94)) PIECES (2,1)) GLOBAL "^ppcaddr(keys(""id""))";
SELECT * FROM ppcaddr ORDER BY id;
SELECT * FROM ppcaddr WHERE street = '45 Oak' ORDER BY id;

-- (2) The AIM transform receives the OUTER delimiter levels as a single string ("spec") in which "/"
--     separates the levels, ":" separates a level's piece number from its delimiter, and "." separates the
--     character codes of a multi-character delimiter. The potential issue: if a delimiter were written into
--     that spec as a literal character, a delimiter that itself IS "/", ":" or "." would be
--     indistinguishable from those structural separators, so the spec would split at the wrong place and
--     extract the wrong piece. This is avoided by encoding every delimiter as its decimal character code,
--     preceded by a one-letter tag ("Z" = byte codes / decode with $ZCHAR, "C" = code points / decode with
--     $CHAR): "/"=47, ":"=58, "."=46 are literal delimiters, so the spec is "1:Z47/1:Z58/1:Z46" -- only
--     digits, the separators and the tag letters, so no delimiter can look like a separator. This test
--     deliberately uses those three characters as the outer delimiters to prove the encoding is
--     collision-free. Each ^ppsep node is "x|<v>.a:b/c"; v extracts to Alpha/Beta/Alpha.
CREATE TABLE ppsep (id INTEGER PRIMARY KEY, v VARCHAR DELIMS ("|","/",":",".") PIECES (2,1,1,1)) GLOBAL "^ppsep(keys(""id""))";
SELECT * FROM ppsep ORDER BY id;
SELECT * FROM ppsep WHERE v = 'Alpha' ORDER BY id;
