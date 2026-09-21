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

-- TC091 : OCTO1112 : a misspelled column name in a keys() that follows "_" is caught at CREATE TABLE time

-- "table_definition.c" validates the EXTRACT string with the same "match_expression()" used to
-- generate the plan. Before YDBOcto#1149 a keys() that followed "_" was not matched, hence never
-- validated, so the CREATE TABLE below was accepted and the misspelled column name surfaced at
-- query time as %YDB-E-LVUNDEF, Undefined local variable: keys("fileidentifier").
-- It must now fail at CREATE TABLE time with ERR_UNKNOWN_COLUMN_NAME.
DROP TABLE IF EXISTS badconcat;
CREATE TABLE badconcat (
	order_number varchar(20),
	file_identifier varchar(1),
	globalname varchar EXTRACT """^GRES""_keys(""fileidentifier"")",
	PRIMARY KEY (order_number, file_identifier)
) GLOBAL "^GREQ(keys(""order_number""),""R"",keys(""file_identifier""))" DELIM "`" READONLY;
