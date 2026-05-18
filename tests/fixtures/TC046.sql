
#################################################################
#								#
# Copyright (c) 2021-2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TC046 : OCTO502 : Allow READWRITE table with column-level DELIM of "" if there is only 1 non-primary-key column
-- Also test that PIECE number (specified as "PIECE 5" below) is ignored when DELIM "" is specified.

CREATE TABLE tmp (id INTEGER PRIMARY KEY, firstName VARCHAR DELIM "" PIECE 5) READWRITE;
-- Insert values where firstName column has default delimiter "|" in it and verify those show up in the column value
-- instead of getting filtered out (which they would if a $piece was done to extract the column value).
INSERT INTO tmp VALUES (1, 'first|1');
INSERT INTO tmp VALUES (2, 'second|2');
SELECT * FROM tmp;
SELECT firstName from tmp;
DROP TABLE tmp;

-- Test that having just one non-key column automatically adds the DELIM "" and that output is same as before.
CREATE TABLE tmp (id INTEGER PRIMARY KEY, firstName VARCHAR) READWRITE;
INSERT INTO tmp VALUES (1, 'first|1');
INSERT INTO tmp VALUES (2, 'second|2');
SELECT * FROM tmp;
SELECT firstName from tmp;
DROP TABLE tmp;

-- YDBOcto#1117 : PIECE must be honored on the only non-key column even when the table-level or column-level
-- DELIM is non-empty. Prior to the fix, the optimization in src/parser/table_definition.c silently replaced
-- column-level PIECE/DELIM with `DELIM ""` whenever the table had a single non-key column with PIECE 1,
-- causing SELECT to return the whole multi-piece global value.
-- The ^test global is populated by the bats @test block before the SQL fixture runs.

-- Variant 1: column-level PIECE only, table-level DELIM (matches #1117 ilabtp_trans.ref_unit_location_group).
DROP TABLE IF EXISTS test;
CREATE TABLE test (id INT PRIMARY KEY, firstname VARCHAR PIECE 1) GLOBAL "^test(keys(""id""))" DELIM "`" READONLY;
SELECT * FROM test;
SELECT firstname FROM test;
DROP TABLE test;

-- Variant 2: column-level PIECE AND column-level DELIM (matches #1117 ilabtp_haem.ref_medical_specialty).
CREATE TABLE test (id INT PRIMARY KEY, firstname VARCHAR DELIM "`" PIECE 1) GLOBAL "^test(keys(""id""))" READONLY;
SELECT * FROM test;
SELECT firstname FROM test;
DROP TABLE test;

-- Variant 3: table-level DELIM only (no column-level PIECE or DELIM). The auto-PIECE 1 added to the only
-- non-key column should use the table-level DELIM and extract piece 1, not be replaced with DELIM "".
CREATE TABLE test (id INT PRIMARY KEY, firstname VARCHAR) GLOBAL "^test(keys(""id""))" DELIM "`" READONLY;
SELECT * FROM test;
SELECT firstname FROM test;
DROP TABLE test;

