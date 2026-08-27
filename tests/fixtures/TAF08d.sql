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

-- TAF08 PART 2 of 2 : the same defect on ^HDR, where the node at START has a real value at the
-- indexed depth. Part 1 (TAF08.sql) covered ^ORDST and is independent of this.

-- Unlike ^ORDST(0), ^HDR(0,0) is not a missing-node NULL, so nothing about NULL handling applies
-- to it: it enters the value dimension as an ordinary subscript. `lo` piece 1 of the START node
-- sorts BELOW every row's value and `hi` piece 2 sorts ABOVE every row's, so if the value
-- dimension is read without applying START, it is MIN(lo) and MAX(hi) that pick it up.
SELECT XECUTE_M_CODE('zwrite ^HDR');

-- hdr : ien START 0 (exclusive), no ENDPOINT, so the rows are ien 1,2 and NOT ien 0.
--   lo is piece 1 of ^HDR(ien,0) -> {mango,apple}, MIN apple, MAX mango
--   hi is piece 2 of ^HDR(ien,0) -> {mango,apple}, MIN apple, MAX mango
--   The values `aaaa` and `zzzz` sitting at ien 0 must appear in NO answer below.
CREATE TABLE hdr (
 ien INTEGER PRIMARY KEY START 0,
 lo VARCHAR(30) GLOBAL "^HDR(keys(""ien""),0)" PIECE 1,
 hi VARCHAR(30) GLOBAL "^HDR(keys(""ien""),0)" PIECE 2
) GLOBAL "^HDR(keys(""ien""))" DELIM "^";

-- The row universe: ien 1 and 2. ien 0 is excluded by START even though ^HDR(0,0) exists.
SELECT * FROM hdr ORDER BY ien;

-- The optimized MIN/MAX, reading the value dimension directly.
SELECT MIN(lo), MAX(lo) FROM hdr;
SELECT MIN(hi), MAX(hi) FROM hdr;

-- The same answers with the optimization disabled by a WHERE clause. Any disagreement with the two
-- queries above is the START node leaking out of the index into an aggregate.
SELECT MIN(lo) FROM hdr WHERE lo IS NOT NULL;
SELECT MAX(lo) FROM hdr WHERE lo IS NOT NULL;
SELECT MIN(hi) FROM hdr WHERE hi IS NOT NULL;
SELECT MAX(hi) FROM hdr WHERE hi IS NOT NULL;

-- And the key column, which does apply START: 1 and 2.
SELECT MIN(ien), MAX(ien) FROM hdr;

-- hdrinc : the SAME global and the SAME range, but declared STARTINCLUDE, which makes the START
-- value part of the range. So ien 0 IS a row here and nothing may be skipped.
--   rows are ien 0,1,2
--   lo -> {aaaa,mango,apple}, MIN aaaa, MAX mango
--   hi -> {zzzz,mango,apple}, MIN apple, MAX zzzz
--   Every one of those differs from `hdr` above wherever ien 0 is the extreme. The cross reference
--   is identical - AIM is handed the same `0:` either way - so the only thing separating these
--   answers from `hdr`'s is how Octo reads the index.
CREATE TABLE hdrinc (
 ien INTEGER PRIMARY KEY START 0 STARTINCLUDE,
 lo VARCHAR(30) GLOBAL "^HDR(keys(""ien""),0)" PIECE 1,
 hi VARCHAR(30) GLOBAL "^HDR(keys(""ien""),0)" PIECE 2
) GLOBAL "^HDR(keys(""ien""))" DELIM "^";

-- The row universe now includes ien 0.
SELECT * FROM hdrinc ORDER BY ien;

-- The optimized MIN/MAX, which must NOT step past ien 0.
SELECT MIN(lo), MAX(lo) FROM hdrinc;
SELECT MIN(hi), MAX(hi) FROM hdrinc;

-- The same answers with the optimization disabled by a WHERE clause, as above.
SELECT MIN(lo) FROM hdrinc WHERE lo IS NOT NULL;
SELECT MAX(hi) FROM hdrinc WHERE hi IS NOT NULL;

-- And the key column: 0 and 2, where `hdr` gave 1 and 2.
SELECT MIN(ien), MAX(ien) FROM hdrinc;
