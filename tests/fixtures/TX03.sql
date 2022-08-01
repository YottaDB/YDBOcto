#################################################################
#								#
# Copyright (c) 2021-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TX03 : OCTO667 : Query that builds more than one cross reference does not TP restart indefinitely

CREATE TABLE names1 (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names1(keys(""ID""))";
CREATE TABLE names2 (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names2(keys(""ID""))";
SELECT COUNT(*) FROM names1 n1, names2 n2 WHERE n1.firstname = 'C' AND n2.lastname = 'D';

