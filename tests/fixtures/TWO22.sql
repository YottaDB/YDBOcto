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

-- TWO22 : YDBOcto#1177 : a key column fixed by = or IN stays inside the bounds of its START, STARTINCLUDE and
-- ENDPOINT keywords, as the table scan and the range (<, >, <=, >=) walks already do. START is exclusive unless
-- STARTINCLUDE is given; ENDPOINT is inclusive. Every answer below must agree with the table scan of its part.

-- ############################################################################################################
-- Part A: an INTEGER key, START 0 ENDPOINT 5. The rows are the subscripts 1 through 5: 3 and 5.
SELECT XECUTE_M_CODE('zwrite ^TWO22a');
CREATE TABLE two22a (k INTEGER PRIMARY KEY START 0 ENDPOINT 5, v VARCHAR PIECE 1) GLOBAL "^TWO22a(keys(""k""))" READONLY;
-- the table scan: 3 and 5
SELECT * FROM two22a ORDER BY k;
-- no row: 0 is START, which is exclusive
SELECT * FROM two22a WHERE k = 0;
-- 5: ENDPOINT is inclusive
SELECT * FROM two22a WHERE k = 5;
-- no row: 6 is past ENDPOINT
SELECT * FROM two22a WHERE k = 6;
-- 3 and 5
SELECT * FROM two22a WHERE k IN (0, 3, 5, 6);
-- the ranges, for comparison: 3 and 5
SELECT * FROM two22a WHERE k >= 0 ORDER BY k;
SELECT * FROM two22a WHERE k <= 6 ORDER BY k;

-- ############################################################################################################
-- Part B: the same global with STARTINCLUDE: 0 is a row.
CREATE TABLE two22b (k INTEGER PRIMARY KEY START 0 ENDPOINT 5 STARTINCLUDE, v VARCHAR PIECE 1) GLOBAL "^TWO22a(keys(""k""))" READONLY;
-- the table scan: 0, 3 and 5
SELECT * FROM two22b ORDER BY k;
-- 0
SELECT * FROM two22b WHERE k = 0;
-- no row: 6 is past ENDPOINT
SELECT * FROM two22b WHERE k = 6;
-- 0 and 5
SELECT * FROM two22b WHERE k IN (0, 5, 6);

-- ############################################################################################################
-- Part C: a VARCHAR key, START '"d"' ENDPOINT '"h"', compared as strings. The rows are e and h.
SELECT XECUTE_M_CODE('zwrite ^TWO22c');
CREATE TABLE two22c (k VARCHAR PRIMARY KEY START '"d"' ENDPOINT '"h"', v VARCHAR PIECE 1) GLOBAL "^TWO22c(keys(""k""))" READONLY;
-- the table scan: e and h
SELECT * FROM two22c ORDER BY k;
-- no row: d is START
SELECT * FROM two22c WHERE k = 'd';
-- h: ENDPOINT is inclusive
SELECT * FROM two22c WHERE k = 'h';
-- no row: k is past ENDPOINT
SELECT * FROM two22c WHERE k = 'k';
-- e and h
SELECT * FROM two22c WHERE k IN ('b', 'd', 'e', 'h', 'k');
-- the range, for comparison: e and h
SELECT * FROM two22c WHERE k >= 'a' ORDER BY k;

-- ############################################################################################################
-- Part D: the same global with STARTINCLUDE: d is a row.
CREATE TABLE two22d (k VARCHAR PRIMARY KEY START '"d"' ENDPOINT '"h"' STARTINCLUDE, v VARCHAR PIECE 1) GLOBAL "^TWO22c(keys(""k""))" READONLY;
-- the table scan: d, e and h
SELECT * FROM two22d ORDER BY k;
-- d
SELECT * FROM two22d WHERE k = 'd';
-- no row: b is below START
SELECT * FROM two22d WHERE k = 'b';
-- d and h
SELECT * FROM two22d WHERE k IN ('b', 'd', 'h', 'k');
-- the range, for comparison: d, e and h
SELECT * FROM two22d WHERE k >= 'a' ORDER BY k;

-- ############################################################################################################
-- Part E: a composite key (g, k), k START 0 ENDPOINT 5, with g fixed, so that k is fixed in place under it.
SELECT XECUTE_M_CODE('zwrite ^TWO22e');
CREATE TABLE two22e (g INTEGER KEY NUM 0, k INTEGER KEY NUM 1 START 0 ENDPOINT 5, v VARCHAR PIECE 1) GLOBAL "^TWO22e(keys(""g""),keys(""k""))" READONLY;
-- the table scan: 1|3, 2|3 and 2|5
SELECT * FROM two22e ORDER BY g, k;
-- no row: 0 is START
SELECT * FROM two22e WHERE g = 1 AND k = 0;
-- no row: 6 is past ENDPOINT
SELECT * FROM two22e WHERE g = 2 AND k = 6;
-- 2|3 and 2|5
SELECT * FROM two22e WHERE g = 2 AND k IN (0, 3, 5, 6);

-- ############################################################################################################
-- Part F: END, a condition that ends the table scan: here, at the first subscript that is not a number. The rows
-- are 1, 5 and 8; the scan stops at a, so neither a nor b is a row.
SELECT XECUTE_M_CODE('zwrite ^TWO22f');
CREATE TABLE two22f (k VARCHAR PRIMARY KEY END "'+keys(""k"")", v VARCHAR PIECE 1) GLOBAL "^TWO22f(keys(""k""))" READONLY;
-- the table scan: 1, 5 and 8
SELECT * FROM two22f ORDER BY k;
-- 5
SELECT * FROM two22f WHERE k = '5';
-- no row: END is true at a
SELECT * FROM two22f WHERE k = 'a';
-- 1 and 8
SELECT * FROM two22f WHERE k IN ('1', '8', 'b');

-- ############################################################################################################
-- Part G: a NUMERIC key, START 1.5 ENDPOINT 7.25: the bounds are compared as numbers. The rows are 2 and 7.25.
SELECT XECUTE_M_CODE('zwrite ^TWO22g');
CREATE TABLE two22g (k NUMERIC PRIMARY KEY START 1.5 ENDPOINT 7.25, v VARCHAR PIECE 1) GLOBAL "^TWO22g(keys(""k""))" READONLY;
-- the table scan: 2 and 7.25
SELECT * FROM two22g ORDER BY k;
-- no row: 1.5 is START
SELECT * FROM two22g WHERE k = 1.5;
-- 7.25: ENDPOINT is inclusive
SELECT * FROM two22g WHERE k = 7.25;
-- no row: 9 is past ENDPOINT
SELECT * FROM two22g WHERE k = 9;
-- 2 and 7.25
SELECT * FROM two22g WHERE k IN (1.5, 2, 7.25, 9);

-- ############################################################################################################
-- Part H: both key columns bounded, g START 1 ENDPOINT 2 and k START 0 ENDPOINT 5, and both fixed: each key is
-- tested against its own bounds inside its own DO. The only row is 2|3.
SELECT XECUTE_M_CODE('zwrite ^TWO22h');
CREATE TABLE two22h (g INTEGER KEY NUM 0 START 1 ENDPOINT 2, k INTEGER KEY NUM 1 START 0 ENDPOINT 5, v VARCHAR PIECE 1)
  GLOBAL "^TWO22h(keys(""g""),keys(""k""))" READONLY;
-- the table scan: 2|3
SELECT * FROM two22h ORDER BY g, k;
-- no row: 1 is g's START
SELECT * FROM two22h WHERE g = 1 AND k = 3;
-- no row: 3 is past g's ENDPOINT
SELECT * FROM two22h WHERE g = 3 AND k = 3;
-- no row: 0 is k's START
SELECT * FROM two22h WHERE g = 2 AND k = 0;
-- 2|3
SELECT * FROM two22h WHERE g = 2 AND k = 3;
-- 2|3: g's IN is tested against g's bounds, k's IN against k's
SELECT * FROM two22h WHERE g IN (1, 2, 3) AND k IN (0, 3, 6);
