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

-- TAF08 PART 1 of 2 : the VistA shape, on ^ORDST. Part 2 (TAF08d.sql) is a separate global and a
-- separate set of tables; the two are kept apart so each can be read on its own.

-- MIN/MAX (the YDBOcto#617 optimization) over a type=4 cross reference whose key range starts at
-- a node that is not a row.

-- Every other read path walks the TABLE's global and uses the cross reference only to advance
-- keys, so it sees the key range the way the table declares it. The YDBOcto#617 optimization does
-- not: for MIN(col)/MAX(col) it $ORDERs the cross reference's VALUE dimension directly and returns
-- the first subscript it lands on. It is the one read path that never applies START.

-- That matters because AIM's key range is INCLUSIVE at both ends while Octo's scan is not: the
-- scan seeds its key with the START value and $ORDERs past it. So the START node itself is in the
-- index and is not a row. Earlier AIM types hid this. They walked down to the VALUE node, so a
-- START node with nothing at the indexed depth was rejected by $DATA for free. type=4 has no such
-- test - the record anchor says any node in range is a row - which is exactly what lets it index a
-- row whose value node is absent as SQL NULL.

-- The data. ^ORDST(0) is the FileMan file header: it holds a value, but it has no `0` node
-- beneath it, so it has nothing at the depth `name` is indexed at. It enters the index in the NULL
-- bucket, which AIM writes as the bare "#" and which sorts BEFORE every real value. Ascending MIN
-- therefore lands on it first and has to step past it. SQL aggregates ignore NULL inputs, so that
-- step is required anyway and the header is dropped as a side effect.
SELECT XECUTE_M_CODE('zwrite ^ORDST');

-- ordstat : ien START 0 (exclusive), no ENDPOINT, so the rows are ien 1,2,3 and NOT ien 0.
--   name is piece 1 of ^ORDST(ien,0) -> {discontinued,complete,pending}, MIN complete, MAX pending
CREATE TABLE ordstat (
 ien INTEGER PRIMARY KEY START 0,
 name VARCHAR(30) GLOBAL "^ORDST(keys(""ien""),0)" PIECE 1,
 vuid VARCHAR(30) GLOBAL "^ORDST(keys(""ien""),""VUID"")" PIECE 1
) GLOBAL "^ORDST(keys(""ien""))" DELIM "^";

-- The row universe as a full table scan sees it: ien 0 is excluded by START, so 1, 2 and 3.
SELECT * FROM ordstat ORDER BY ien;

-- MIN/MAX on a non-key column: the YDBOcto#617 optimization, reading the value dimension directly.
SELECT MIN(name), MAX(name) FROM ordstat;

-- The same two answers computed WITHOUT that optimization - a WHERE clause disables it, so these
-- go through the ordinary key walk, which does apply START. The two must agree.
SELECT MIN(name) FROM ordstat WHERE name IS NOT NULL;
SELECT MAX(name) FROM ordstat WHERE name IS NOT NULL;

-- MIN/MAX on the KEY column. This does not use a cross reference at all - it seeds from START and
-- $ORDERs past it - so it is the contrast case: 1 and 3, not 0 and 3.
SELECT MIN(ien), MAX(ien) FROM ordstat;

-- Which rows are in the NULL bucket, asked through the key walk. ien 0 is in that bucket in the
-- index and is not a row, so this must not report it.
SELECT * FROM ordstat WHERE name IS NULL ORDER BY ien;

-- Aggregates that are never optimized, for the row counts: 3 rows, 3 non-NULL names, 1 non-NULL vuid.
SELECT COUNT(*), COUNT(name), COUNT(vuid) FROM ordstat;
