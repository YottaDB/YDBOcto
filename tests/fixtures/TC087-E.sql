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
-- TC088 Section A : YDBOcto#763/#764 : PIECE + SUBSTR counts CHARACTERS under ydb_chset=UTF-8
--
-- ^u1 piece 1 holds the value to extract from and piece 2 names the case, so every row below
-- is identified by its "label" rather than by its id:
--   mixed  "Xé日Y"   X(1 byte) é(2 bytes) 日(3 bytes) Y(1 byte) = 4 characters, 7 bytes
--   cjk    "日本語"   three 3-byte characters                    = 3 characters, 9 bytes
--   ascii  "abcdef"  pure ASCII, where characters and bytes coincide
--   short  "é"       a single 2-byte character; SUBSTR 1-3 runs past the end
--   empty  ""        an empty piece; the extraction is empty, i.e. SQL NULL
--
-- The "char4" column is the decisive one: on the "mixed" value the 4th CHARACTER is "Y",
-- whereas the 4th BYTE is the middle of the 日 sequence. Getting "Y" proves $EXTRACT is
-- counting characters. Under ydb_chset=M every column below would differ, which is why the
-- whole subtest is skipped unless the run picked UTF-8.
CREATE TABLE u1 (
	id	INTEGER PRIMARY KEY,
	label	VARCHAR PIECE 2,
	head3	VARCHAR PIECE 1 SUBSTR 1-3,
	first1	VARCHAR PIECE 1 SUBSTR 1,
	mid	VARCHAR PIECE 1 SUBSTR 2-3,
	char4	VARCHAR PIECE 1 SUBSTR 4-4
) GLOBAL "^u1(keys(""id""))" READONLY;

-- Full extraction matrix. Expect head3: Xé日 / 日本語 / abc / é / NULL
--                        and char4: Y / NULL / d / NULL / NULL
select label, head3, first1, mid, char4 from u1 order by label;

-- Equality against a multi-byte literal in the SQL text.
select label from u1 where head3 = 'Xé日';
select label from u1 where first1 = '日';

-- A SUBSTR range starting beyond the end of a short value yields SQL NULL, and an empty
-- piece does too; both must appear in IS NULL and neither in IS NOT NULL.
select label from u1 where char4 is null order by label;
select label from u1 where head3 is not null order by label;

-- The extraction composes with an ORDER BY on the extracted value itself.
select label, head3 from u1 where head3 is not null order by head3;
