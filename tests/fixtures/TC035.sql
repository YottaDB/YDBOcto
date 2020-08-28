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

-- TC035 : OCTO483 : CREATE TABLE allows column-level DELIM qualifier

CREATE TABLE DELIMNAMES (id INTEGER PRIMARY KEY, firstName VARCHAR(30) DELIM (9), lastName VARCHAR(30), middleInitial VARCHAR(1), age INTEGER) GLOBAL "^delimnames(keys(""id""))";

SELECT * FROM delimnames;
SELECT * FROM delimnames WHERE firstname = 'Zero';
SELECT * FROM delimnames WHERE firstname != 'Zero';

