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
-- TC089 : OCTO1146 : Section F : an EXTRACT that cannot call M code gets no label

CREATE TABLE tc089f (
	id INTEGER PRIMARY KEY,
	firstname VARCHAR(30),
	lastname VARCHAR(30),
	plain VARCHAR EXTRACT "$PIECE($GET(^names(keys(""id""))),""|"",1)",
	called VARCHAR EXTRACT "$$col^TC089(values(""firstname""))"
) GLOBAL "^names(keys(""id""))" READONLY;

SELECT id, plain, called FROM tc089f WHERE id < 2;
