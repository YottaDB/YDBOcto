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

-- TC089 : OCTO1146 : Section D : nested EXTRACT columns each report their own name

CREATE TABLE tc089d (
	id INTEGER PRIMARY KEY,
	firstname VARCHAR(30),
	lastname VARCHAR(30),
	nickname VARCHAR EXTRACT "$$col^TC089(values(""firstname""))",
	fullname VARCHAR EXTRACT "$$col^TC089(values(""nickname""))"
) GLOBAL "^names(keys(""id""))" READONLY;

-- "fullname" references "nickname", which is itself an EXTRACT column, so both expressions are
-- emitted as ONE M expression. The inner routine must still see "nickname" and the outer one
-- "fullname" -- i.e. the expected value is "<nickname><fullname>", never "<fullname><fullname>".
SELECT id, fullname FROM tc089d WHERE id < 2;

-- Both columns in one query: the inner label is shared with the standalone "nickname" reference.
SELECT id, nickname, fullname FROM tc089d WHERE id = 0;
