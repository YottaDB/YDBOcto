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

-- TC093 : OCTO1196 : EXTRACT that calls a SQL function survives an auto upgrade of its text definition

-- Each EXTRACT column below tests a different kind of SQL function call text:
--   absid    : a plain column argument
--   quoted   : a string literal holding a double quote, which M code text would need to escape
--   idtext   : a type cast argument
--   tblqual  : a table-qualified column argument
--   dblquote : a double-quoted, case-sensitive column argument
--   mcode    : an M code EXTRACT, which was always stored correctly, for comparison
CREATE TABLE TC093 (
	id INTEGER PRIMARY KEY, firstName VARCHAR, "lastName" VARCHAR,
	absid INTEGER EXTRACT ABS(id),
	quoted VARCHAR EXTRACT CONCAT(firstName, '"'),
	idtext VARCHAR EXTRACT CONCAT(id::varchar, 'a'),
	tblqual VARCHAR EXTRACT CONCAT(TC093.firstName, '-'),
	dblquote VARCHAR EXTRACT CONCAT("lastName", '!'),
	mcode INTEGER EXTRACT "$$ABS^%ydboctosqlfunctions(keys(""id""))"
) GLOBAL "^names(keys(""id""))" READONLY;
SELECT * FROM TC093;
