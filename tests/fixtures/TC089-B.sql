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

-- TC089 : OCTO1146 : Section B : same accessors from the "EXTRACT <sql function call>" form

CREATE FUNCTION TC089CTX(VARCHAR) RETURNS VARCHAR AS $$ctx^TC089;

CREATE TABLE tc089b (
	id INTEGER PRIMARY KEY,
	firstname VARCHAR(30),
	lastname VARCHAR(30),
	fnwhoami VARCHAR EXTRACT TC089CTX(firstname)
) GLOBAL "^names(keys(""id""))" READONLY;

-- An EXTRACT that is a SQL function call goes through a different emission path
-- ("tmpl_print_expression") than the M-string form in Section A, but must set up the same context.
SELECT id, fnwhoami FROM tc089b WHERE id < 2;
