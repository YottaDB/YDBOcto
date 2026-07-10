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
-- TX17 : YDBOcto#763/#764 : AIM cross reference over a multi-byte SUBSTR column (UTF-8 only)
--
-- TC088 shows the READ side extracts characters rather than bytes. This checks that the AIM
-- index agrees: AIM stores what "SubstrXform" returns, and that transform runs $EXTRACT in the
-- same process (hence the same chset) as the read side. If the two ever disagreed, an indexed
-- lookup would silently miss rows that a full scan finds.
--
-- ^ux piece 1 is the value, piece 2 names the case:
--   mixed  "Xé日Y"   1-, 2- and 3-byte characters in one value
--   cjk    "日本語"   all 3-byte characters
--   ascii  "abcdef"  characters and bytes coincide
--   empty  ""        extraction is empty, i.e. SQL NULL
CREATE TABLE ux (
	id	INTEGER PRIMARY KEY,
	label	VARCHAR PIECE 2,
	head3	VARCHAR PIECE 1 SUBSTR 1-3
) GLOBAL "^ux(keys(""id""))" READONLY;

-- Builds the cross reference, then answers from it. The literal is multi-byte, so this only
-- matches if the indexed subscript was built with character (not byte) extraction.
select label from ux where head3 = 'Xé日';
select label from ux where head3 = '日本語';

-- MIN/MAX read straight off the AIM index (YDBOcto#617). Ordering is by the "#"-prefixed
-- subscript, i.e. M collation over the UTF-8 bytes: X(0x58) < a(0x61) < 日(0xE697A5).
select MIN(head3) as LO, MAX(head3) as HI from ux;

-- The empty extraction is SQL NULL and is indexed under the same "#" marker a plain string
-- column uses, so an indexed IS NULL finds it and IS NOT NULL excludes it.
select label from ux where head3 is null;
select label from ux where head3 is not null order by label;
