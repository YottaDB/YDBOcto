#################################################################
#								#
# Copyright (c) 2020-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TC018 : OCTO483 : CREATE TABLE works with regular characters as DELIM qualifier and validate xrefs

CREATE TABLE DELIMNAMESU (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30), middleInitial VARCHAR(1), age INTEGER) DELIM '_' GLOBAL "^delimnamesu(keys(""ID""))";
CREATE TABLE DELIMNAMESP (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30), middleInitial VARCHAR(1), age INTEGER) DELIM '|' GLOBAL "^delimnamesp(keys(""ID""))";

SELECT * FROM delimnamesu;
SELECT * FROM delimnamesp;
SELECT * FROM delimnamesu WHERE firstname = 'Zero';
SELECT * FROM delimnamesp WHERE firstname = 'Zero';

