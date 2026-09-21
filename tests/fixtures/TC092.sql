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
-- TC092 : OCTO1116 : keys() in an END is expanded when it follows an M operator

-- "episodes" places keys("internal_episode_record_no") after the "+" of the END expression
-- "'+keys(...)", i.e. stop the scan at the first subscript that is not numeric. Before YDBOcto#1149
-- a keys() was expanded only when it was preceded by "(" or ",", so this reference was emitted as is
-- and the query failed with
-- %YDB-E-LVUNDEF, Undefined local variable: keys("internal_episode_record_no").
-- "episodes2" is the same END written as "'+(keys(...))". That form was already expanded before
-- YDBOcto#1149 (it was the workaround reported in the issue) and must keep working. Both tables
-- must return the same rows.
DROP TABLE IF EXISTS episodes;
CREATE TABLE episodes (
	internal_episode_record_no varchar(20) END "'+keys(""internal_episode_record_no"")",
	episode_status varchar(10) PIECE 1,
	PRIMARY KEY (internal_episode_record_no)
) GLOBAL "^GCND(keys(""internal_episode_record_no""),""A"")" DELIM "`" READONLY;
DROP TABLE IF EXISTS episodes2;
CREATE TABLE episodes2 (
	internal_episode_record_no varchar(20) END "'+(keys(""internal_episode_record_no""))",
	episode_status varchar(10) PIECE 1,
	PRIMARY KEY (internal_episode_record_no)
) GLOBAL "^GCND(keys(""internal_episode_record_no""),""A"")" DELIM "`" READONLY;

select * from episodes;
select * from episodes2;
