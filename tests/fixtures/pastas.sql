#################################################################
#								#
# Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

CREATE TABLE names4 (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30), favoritePasta VARCHAR(30)) GLOBAL "^names4(keys(""id""))";
CREATE TABLE pastas (id INTEGER PRIMARY KEY, pastaName VARCHAR(30)) GLOBAL "^pastas(keys(""id""))";
