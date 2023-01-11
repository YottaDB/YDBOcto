#################################################################
#								#
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TII11 : OCTO910 : Test INSERT INTO with VALUES as source table avoids unnecessary str2mval()/mval2str() calls

CREATE TABLE tmp (id1 INTEGER PRIMARY KEY, id2 INTEGER, id3 INTEGER);
INSERT INTO tmp VALUES (1, 2, 3);

