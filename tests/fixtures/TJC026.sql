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

-- TJC026 : OCTO1127 : Bind parameters whose value is NULL (JDBC "setNull()")
-- Before YDBOcto#1127, every prepared statement with a setNull() parameter failed at the Bind message with
--   ERROR: Bind: invalid number of result column format codes: -256 is out of range (min: 0, max: 32767)
-- as rocto misread the Bind message after the NULL value (length -1, no value bytes). This test verifies that reads and
-- writes with setNull() parameters now work, that the parameter is SQL NULL (not an empty string), and that a text value
-- followed by a NULL value is stored exactly.

CREATE TABLE TJC026 (id INTEGER PRIMARY KEY, val INTEGER, name VARCHAR) GLOBAL "^TJC026";
INSERT INTO TJC026 VALUES (1, 10, 'PAT'), (2, 20, 'PATIENT'), (3, NULL, NULL);
SELECT * FROM TJC026 ORDER BY id;
