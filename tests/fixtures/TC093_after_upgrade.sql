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

-- TC093 : OCTO1196 : EXTRACT that calls a SQL function survives an auto upgrade of its text definition

-- These queries read the EXTRACT columns after the auto upgrade re-parsed the table's text definition.
-- They should give the same results as before the upgrade.
SELECT * FROM TC093;
SELECT id, quoted, dblquote FROM TC093 WHERE absid = 3;
