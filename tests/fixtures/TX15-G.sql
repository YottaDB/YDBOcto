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
-- Multi-byte UTF-8 delimiters in the AIM (piece-of-piece) path (YDBOcto#1108). A literal delimiter is
-- encoded into the AIM transform spec by its BYTE codes and rebuilt with $ZCHAR, so a multi-byte UTF-8
-- delimiter is reproduced exactly (rebuilding those byte codes with $CHAR would treat each byte as a Unicode
-- code point and produce a different string in UTF-8 mode). Each table below is READONLY (piece-of-piece),
-- so every WHERE builds and reads the AIM cross-reference. SELECT * (the direct read-side $PIECE) is shown
-- first as a baseline.

-- (1) Multi-byte UTF-8 CHARACTER as a delimiter. "䀀" is U+4000 (bytes E4.80.80). ^ppu1 nodes are
--     "<v1>䀀<v2>|<v3>䀀<v4>": ascii "|" and the multi-byte "䀀" appear as inner AND outer delimiters.
CREATE TABLE ppu_char (id INTEGER PRIMARY KEY,
	v1 VARCHAR DELIMS ("|","䀀") PIECES (1,1),
	v2 VARCHAR DELIMS ("|","䀀") PIECES (1,2),
	v3 VARCHAR DELIMS ("|","䀀") PIECES (2,1),
	v4 VARCHAR DELIMS ("|","䀀") PIECES (2,2)
) GLOBAL "^ppu1(keys(""id""))";
SELECT * FROM ppu_char ORDER BY id;
SELECT * FROM ppu_char WHERE v1 = 'a';
SELECT * FROM ppu_char WHERE v2 = 'b';
SELECT * FROM ppu_char WHERE v3 = 'c';
SELECT * FROM ppu_char WHERE v4 = 'd';
-- "䀀" as the INNER (first) delimiter, ascii "|" as the outer:
CREATE TABLE ppu_charin (id INTEGER PRIMARY KEY,
	w1 VARCHAR DELIMS ("䀀","|") PIECES (1,1),
	w2 VARCHAR DELIMS ("䀀","|") PIECES (2,1)
) GLOBAL "^ppu1(keys(""id""))";
SELECT * FROM ppu_charin ORDER BY id;
SELECT * FROM ppu_charin WHERE w2 = 'b';

-- (2) Multi-byte UTF-8 STRING as a delimiter. "上下" is two multi-byte characters (U+4E0A U+4E0B, bytes
--     E4.B8.8A E4.B8.8B). ^ppu2 nodes are "<a>上下<b>|<c>上下<d>": inner "|", outer multi-char "上下".
CREATE TABLE ppu_str (id INTEGER PRIMARY KEY,
	a VARCHAR DELIMS ("|","上下") PIECES (1,1),
	b VARCHAR DELIMS ("|","上下") PIECES (2,2)
) GLOBAL "^ppu2(keys(""id""))";
SELECT * FROM ppu_str ORDER BY id;
SELECT * FROM ppu_str WHERE a = 'p';
SELECT * FROM ppu_str WHERE b = 's';

-- (3) MIX of a multi-byte UTF-8 character and an ascii character in one delimiter. The delimiter "Y上" is
--     ascii "Y" (byte 89) followed by "上" (bytes E4.B8.8A). ^ppu3 nodes are "<c>Y上<d>|<e>Y上<f>".
CREATE TABLE ppu_mix (id INTEGER PRIMARY KEY,
	c VARCHAR DELIMS ("|","Y上") PIECES (1,1),
	d VARCHAR DELIMS ("|","Y上") PIECES (1,2)
) GLOBAL "^ppu3(keys(""id""))";
SELECT * FROM ppu_mix ORDER BY id;
SELECT * FROM ppu_mix WHERE c = 'm';
SELECT * FROM ppu_mix WHERE d = 'n';
