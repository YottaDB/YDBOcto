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
-- SKIP and SKIPCONDITION accept arbitrary M expressions (not just literals).
-- The codegen for SKIP emits each comma-separated piece verbatim inside
-- $SELECT(<key>=<piece>:1,...); the codegen for SKIPCONDITION emits the
-- expression verbatim inside QUIT:(<expr>) after keys(...) substitution --
-- so any valid M expression is honored at runtime.
--
-- Uses the shared ^skiptest seed:
--   1, 2, 99, 100, 101, 200, "A", "B", "C", "x,y".
--
-- SKIP "$LENGTH(""ab"")" evaluates to 2 at runtime, so the subscript whose
-- string value equals 2 (i.e. id=2) is dropped.  The first SELECT proves
-- that SKIP'd value is computed by M, not parsed by Octo.
CREATE TABLE skiptest_s7a (
  id VARCHAR(8) PRIMARY KEY SKIP "$LENGTH(""ab"")",
  name VARCHAR(64)
) GLOBAL "^skiptest";
SELECT * FROM skiptest_s7a ORDER BY id;

-- SKIPCONDITION "$LENGTH(keys(""id""))>2" -- arbitrary M boolean expression
-- referencing the key column.  M $LENGTH of the subscript value is computed
-- per row; rows with subscript length > 2 are dropped.  Of the ten seed
-- subscripts, the three-character ones ("100", "101", "200", "x,y") are
-- filtered out, leaving 1, 2, 99, "A", "B", "C".
CREATE TABLE skiptest_s7b (
  id VARCHAR(8) PRIMARY KEY SKIPCONDITION "$LENGTH(keys(""id""))>2",
  name VARCHAR(64)
) GLOBAL "^skiptest";
SELECT * FROM skiptest_s7b ORDER BY id;

-- Combined: SKIP value is a parenthesized M arithmetic expression and
-- SKIPCONDITION is a comparison against $LENGTH of another M expression.
-- Confirms the two filters compose when each is an M expression rather
-- than a literal.  Note the parentheses around the SKIP expression --
-- because the SKIP piece is emitted verbatim on the right-hand side of
-- "<key>=<piece>", M's strict left-to-right evaluation would otherwise
-- bind the "=" before the trailing arithmetic.  The "1+1" form below
-- evaluates to 2 at runtime so the row with id=2 is dropped.  The
-- SKIPCONDITION additionally drops the four three-character subscripts
-- ("100", "101", "200", "x,y"), leaving 1, 99, "A", "B", "C".
CREATE TABLE skiptest_s7c (
  id VARCHAR(8) PRIMARY KEY SKIP "(1+1)" SKIPCONDITION "$LENGTH(keys(""id""))>2",
  name VARCHAR(64)
) GLOBAL "^skiptest";
SELECT * FROM skiptest_s7c ORDER BY id;
