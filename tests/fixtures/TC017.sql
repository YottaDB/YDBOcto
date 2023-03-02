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

-- TC017 : OCTO483 : CREATE TABLE allows other non-printable characters (carriage return) in DELIM qualifier and validate xrefs

CREATE TABLE DELIMNAMES (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30), middleInitial VARCHAR(1), age INTEGER) DELIM (13) GLOBAL "^delimnames(keys(""ID""))";

SELECT * FROM delimnames;
SELECT * FROM delimnames WHERE firstname = 'Zero';

