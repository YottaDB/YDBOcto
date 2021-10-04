#################################################################
#								#
# Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

CREATE TABLE composite (
	id0 INTEGER PRIMARY KEY,
	id1 INTEGER KEY NUM 1,
	id2 INTEGER KEY NUM 2,
	id3 INTEGER KEY NUM 3,
	id4 INTEGER KEY NUM 4,
	id5 INTEGER KEY NUM 5,
	id6 INTEGER KEY NUM 6,
	id7 INTEGER KEY NUM 7,
	name VARCHAR(20)
	)
GLOBAL "^composite(keys(""id0""),keys(""id1""),keys(""id2""),keys(""id3""),keys(""id4""),keys(""id5""),keys(""id6""),keys(""id7""))";

