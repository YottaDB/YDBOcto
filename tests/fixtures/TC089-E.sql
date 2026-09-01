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

-- TC089 : OCTO1146 : Section E : context survives an error, and is empty outside an EXTRACT

CREATE FUNCTION TC089NOCTX(VARCHAR) RETURNS VARCHAR AS $$nocontext^TC089;

CREATE TABLE tc089e (
	id INTEGER PRIMARY KEY,
	firstname VARCHAR(30),
	lastname VARCHAR(30),
	trapped VARCHAR EXTRACT "$$trap^TC089(values(""firstname""))",
	raiser VARCHAR EXTRACT "$$boom^TC089(values(""firstname""))",
	after VARCHAR EXTRACT "$$col^TC089(values(""lastname""))"
) GLOBAL "^names(keys(""id""))" READONLY;

-- An error raised and caught inside the EXTRACT routine: the handler runs below the label that
-- NEWed the context, so it still reports "trapped"; the next column then reports "after".
SELECT id, trapped, after FROM tc089e WHERE id < 2;

-- An error that is NOT caught fails the query ...
SELECT id, raiser FROM tc089e WHERE id = 0;

-- ... but nothing leaked out of it: this query reports its own column, not "raiser".
SELECT id, after FROM tc089e WHERE id < 2;

-- A function call that is not attached to a column is not under any label, so it sees no context.
SELECT id, TC089NOCTX(firstname) FROM tc089e WHERE id < 2;
