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

-- TC092 : OCTO1116 : a misspelled column name in an END keys() that follows an M operator is caught
--                    at CREATE TABLE time

-- "validate_start_end_keyword.c" validates the END string with the same "match_expression()" used to
-- generate the plan. Before YDBOcto#1149 a keys() that followed "+" was not matched, hence never
-- validated, so the CREATE TABLE below was accepted and the misspelled column name surfaced at query
-- time as %YDB-E-LVUNDEF, Undefined local variable: keys("internal_episode_record").
-- It must now fail at CREATE TABLE time with ERR_UNKNOWN_COLUMN_NAME.
DROP TABLE IF EXISTS badend;
CREATE TABLE badend (
	internal_episode_record_no varchar(20) END "'+keys(""internal_episode_record"")",
	PRIMARY KEY (internal_episode_record_no)
) GLOBAL "^GCND(keys(""internal_episode_record_no""),""A"")" DELIM "`" READONLY;
