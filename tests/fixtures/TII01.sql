#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TIIT01 : INSERT INTO with firstName='Zero'

CREATE TABLE names2 (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30));
INSERT INTO names2 (SELECT * FROM names WHERE firstName='Zero');
SELECT * FROM names2;
DROP TABLE names2;

