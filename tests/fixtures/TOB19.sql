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

-- TOB19 : OCTO211 : Validate that ORDER BY column number usage in a query which goes through DNF expansion doesn't result in an
-- 		     assert failure.
-- Review comment which led to this issue fix: https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1390890878
SELECT (SELECT 1) FROM (SELECT (SELECT 1) FROM names WHERE (true OR false) ORDER BY 1)n1 WHERE (true OR false) ORDER BY 1;
-- Simplified form of the above query which also failed for the same reason
SELECT 1 FROM (SELECT (SELECT 1) ORDER BY 1)n1 WHERE (true OR false);
