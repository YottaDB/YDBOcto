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

-- TPGC010 : OCTO1158 : LIKE with a bind parameter pattern, in cases that a JDBC client cannot produce:
--   1. a query where $2 occurs before $1,
--   2. Bind messages with no parameter format codes, and with one per parameter (including a binary value),
--   3. a table dropped by another connection between the Parse and the Bind of a prepared statement.
-- TJC025 (JDBC) tests the rest of the LIKE bind parameter cases.

CREATE TABLE TPGC010 (id INTEGER PRIMARY KEY, v VARCHAR) GLOBAL "^TPGC010";
INSERT INTO TPGC010 VALUES (1, 'PAT'), (2, 'PATX'), (3, 'ZX'), (4, 'AX');
CREATE TABLE TPGC010B (id INTEGER PRIMARY KEY, v VARCHAR) GLOBAL "^TPGC010B";
INSERT INTO TPGC010B VALUES (1, 'PAT');
SELECT * FROM TPGC010 ORDER BY id;
