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

-- TC061 : OCTO633 : EXTRACT accepts non-key columns

-- Table with literal delimiter (default '|')
CREATE TABLE extractnames (
	id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30),  age INTEGER,
	fullname VARCHAR EXTRACT "$$^FULLNAME(values(""FIRSTNAME""),values(""LASTNAME""))"
) GLOBAL "^names(keys(""ID""))";

select * from extractnames;
select fullname from extractnames;

-- Table with non-literal delimiter at table level
CREATE TABLE delimnames (
	id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30),  middleInitial VARCHAR(1), age INTEGER,
	fullname VARCHAR EXTRACT "$$^FULLNAME(values(""FIRSTNAME""),values(""LASTNAME""))"
) DELIM (123, 9, 124, 9, 125) GLOBAL "^delimnames(keys(""ID""))";

select * from extractnames;
select fullname from delimnames;

-- Table with non-literal delimiter at column level
CREATE TABLE delimnamescol (
	id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30) DELIM (13), middleInitial VARCHAR(1), age INTEGER,
	fullname VARCHAR EXTRACT "$$^FULLNAME(values(""FIRSTNAME""),values(""LASTNAME""))"
) GLOBAL "^delimnamescol(keys(""ID""))";

select * from extractnames;
select fullname from delimnamescol;
