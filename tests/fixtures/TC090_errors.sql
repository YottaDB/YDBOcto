#################################################################
#								#
# Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TC090 : OCTO1149 : a misspelled column name after a ":" is caught at CREATE TABLE time

-- "table_definition.c" validates the EXTRACT string with the same "match_expression()" used to
-- generate the plan. Before YDBOcto#1149 a keys()/values() that followed a ":" was not matched,
-- hence never validated, so both CREATE TABLEs below were accepted and the misspelled column name
-- surfaced at query time as %YDB-E-LVUNDEF, Undefined local variable: keys("agesource").
-- Both must now fail at CREATE TABLE time with ERR_UNKNOWN_COLUMN_NAME.
DROP TABLE IF EXISTS badkeys;
CREATE TABLE badkeys (
	id integer,
	age_source integer,
	age varchar(6) EXTRACT "$S(keys(""age_source"")<28:keys(""agesource""),1:""old"")",
	PRIMARY KEY (id, age_source)
) GLOBAL "^ref(keys(""id""),keys(""age_source""))" DELIM "`" READONLY;
DROP TABLE IF EXISTS badvalues;
CREATE TABLE badvalues (
	id integer PRIMARY KEY,
	refrange varchar(13) PIECE 1,
	disp varchar(20) EXTRACT "$S(values(""refrange"")[""{"":values(""refrang""),1:values(""refrange""))"
) GLOBAL "^ref(keys(""id""))" DELIM "`" READONLY;
