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

-- Reproduction of YDBOcto#1128: a column defined with the EXTRACT keyword whose
-- expression embeds keys(...) issued a ZYSQLNULLNOTVALID runtime error when the
-- column was on the unmatched (NULL-valued) side of a LEFT JOIN:
--   %YDB-E-ZYSQLNULLNOTVALID, $ZYSQLNULL cannot be used as ... gvn subscript
--
-- "reqs" is the LEFT (driver) table.  "results" is the RIGHT (readonly) table.
-- Two columns of "results" read piece 2 of the same node, ^X(reg_num), in
-- different ways:
--   * specnum uses the column-level GLOBAL/PIECE keywords.  This access is
--     guarded against NULL keys and correctly returns NULL for an unmatched row.
--   * ext_specnum uses EXTRACT.  Before the fix this access was emitted
--     unguarded, so an unmatched LEFT JOIN row (all of whose key lvns are set
--     to $ZYSQLNULL) evaluated $PIECE($GET(^X($ZYSQLNULL)),"|",2) and errored.

CREATE TABLE reqs (reg_num VARCHAR(20) PRIMARY KEY, patient VARCHAR(30)) GLOBAL "^X(keys(""reg_num""))" DELIM "|" READONLY;

CREATE TABLE results (
	reg_num VARCHAR(20),
	test_index INTEGER,
	test_code VARCHAR(20) PIECE 1,
	specnum VARCHAR(20) GLOBAL "^X(keys(""reg_num""))" DELIM "|" PIECE 2,
	ext_specnum VARCHAR(20) EXTRACT "$PIECE($GET(^X(keys(""reg_num""))),""|"",2)",
	PRIMARY KEY (reg_num, test_index)
) GLOBAL "^Y(keys(""reg_num""),keys(""test_index""))" DELIM "|" READONLY;

-- Show the contents of both tables so a human reviewer can see the data the JOIN runs on.
-- reqs has REG1 and REG2; results has a row only for REG1.
SELECT * FROM reqs;
SELECT * FROM results;

-- REG2 has no ^Y row, so it is an unmatched right row: results.* must come back NULL.
SELECT * FROM reqs r LEFT JOIN results t ON r.reg_num = t.reg_num ORDER BY r.reg_num;
