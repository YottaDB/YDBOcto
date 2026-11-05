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

-- TOB26 : OCTO1179 : ORDER BY on a BOOLEAN key column of a READONLY table sorts
-- Crosschecked against Postgres, whose tob26ro and tob26rw hold the same rows.

-- ############################################################################################################
-- Part A: the READONLY table. M orders the k2 subscripts under each k1 numbers first, then strings, upper case before
-- lower case: under 1, 1 (true) then "f" (false); under 2, 0 (false) then "Y" (true); under 3, "T" (true) then "no"
-- (false). Under 1 and 3 the FOR loop of k2 gives true before false, so an ORDER BY k2 must sort.
-- The rows, in M order: 1|t|a, 1|f|b, 2|f|c, 2|t|d, 3|t|e and 3|f|g
SELECT * FROM TOB26ro;
-- [sort] 1|f|b, then 1|t|a
SELECT * FROM TOB26ro WHERE k1 = 1 ORDER BY k2;
-- [sort] 1|t|a, then 1|f|b
SELECT * FROM TOB26ro WHERE k1 = 1 ORDER BY k2 DESC;
-- [sort] 3|f|g, then 3|t|e
SELECT * FROM TOB26ro WHERE k1 = 3 ORDER BY k2;
-- [sort] 1|f|b, 1|t|a, 2|f|c, 2|t|d, 3|f|g, then 3|t|e
SELECT * FROM TOB26ro ORDER BY k1, k2;

-- ############################################################################################################
-- Part B: the READWRITE table, with the same rows. Octo stores k2 as 0 or 1, which M orders as boolean order (false
-- before true), so an ORDER BY k2 does not sort. The answers are those of Part A.
-- The rows, in M order: 1|f|b, 1|t|a, 2|f|c, 2|t|d, 3|f|g and 3|t|e
SELECT * FROM TOB26rw;
-- [no sort] 1|f|b, then 1|t|a
SELECT * FROM TOB26rw WHERE k1 = 1 ORDER BY k2;
-- [no sort] 1|t|a, then 1|f|b
SELECT * FROM TOB26rw WHERE k1 = 1 ORDER BY k2 DESC;
-- [no sort] 3|f|g, then 3|t|e
SELECT * FROM TOB26rw WHERE k1 = 3 ORDER BY k2;
-- [no sort] 1|f|b, 1|t|a, 2|f|c, 2|t|d, 3|f|g, then 3|t|e
SELECT * FROM TOB26rw ORDER BY k1, k2;
