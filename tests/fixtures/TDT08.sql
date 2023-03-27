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
-- TDT08 : OCTO802 : Dropping and Recreating the same table with different delimiters resulted in incorrect query results

CREATE TABLE names (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names";
-- Supposed to see one row
select * from names where FIRSTNAME = 'Joey';
drop table names keepdata;
CREATE TABLE names (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names" DELIM "^";
-- Supposed to see no rows (data has | delimiter)
select * from names where FIRSTNAME = 'Joey';
-- Print the contents according to the new definition
select id,'"',firstname,'"' from names;
