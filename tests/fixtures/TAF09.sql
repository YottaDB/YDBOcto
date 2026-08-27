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

-- TAF09 : YDBAIM#96 : MIN/MAX with START and SKIP together on the key column.
--
-- TAF08 covers START alone: AIM's inclusive range puts the START node in the index, and the
-- MIN/MAX read of the value dimension must step past it. With SKIP as well the read has to walk
-- the keys behind a value until one survives both: key 0 is the START entry and key 2 is skipped,
-- so of ^HDR(0,0)="aaaa^zzzz", (1,0)="mango^mango", (2,0)="apple^apple", (3,0)="banana^banana"
-- only keys 1 and 3 are rows.
CREATE TABLE hdrskip (
 ien INTEGER PRIMARY KEY START 0 SKIP '2',
 lo VARCHAR(30) GLOBAL "^HDR(keys(""ien""),0)" PIECE 1,
 hi VARCHAR(30) GLOBAL "^HDR(keys(""ien""),0)" PIECE 2
) GLOBAL "^HDR(keys(""ien""))" DELIM "^";
-- The rows as the table scan sees them.
SELECT * FROM hdrskip ORDER BY ien;
-- WHERE col IS NOT NULL disables the YDBOcto#617 read, so these are the answers to match.
SELECT MIN(lo), MAX(lo) FROM hdrskip WHERE lo IS NOT NULL;
SELECT MIN(hi), MAX(hi) FROM hdrskip WHERE hi IS NOT NULL;
