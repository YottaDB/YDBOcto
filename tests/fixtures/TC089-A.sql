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

-- TC089 : OCTO1146 : Section A : both accessors from a plain M-string EXTRACT column

CREATE TABLE tc089a (
	id INTEGER PRIMARY KEY,
	firstname VARCHAR(30),
	lastname VARCHAR(30),
	whoami VARCHAR EXTRACT "$$ctx^TC089(values(""firstname""))"
) GLOBAL "^names(keys(""id""))" READONLY;

-- "whoami" names the table and column it is being computed for, with no help from the DDL.
SELECT id, whoami FROM tc089a WHERE id < 2;
