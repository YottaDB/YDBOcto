#################################################################
#								#
# Copyright (c) 2019-2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

CREATE TABLE composite (
	id0 INTEGER,
	id1 INTEGER,
	id2 INTEGER,
	id3 INTEGER,
	id4 INTEGER,
	id5 INTEGER,
	id6 INTEGER,
	id7 INTEGER,
	name VARCHAR(20),
	PRIMARY KEY (id0, id1, id2, id3, id4, id5, id6, id7)
	)
GLOBAL "^composite";

