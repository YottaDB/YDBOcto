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

-- TERR054 : OCTO1109 : Test ERR_SKIP_NEEDS_KEY_COLUMN error
-- SKIP and SKIPCONDITION only make sense on KEY columns since they filter
-- iterations of the $ORDER FOR loop for that key. Specifying them on a
-- non-key column should be rejected.

-- SKIP on a non-key column
CREATE TABLE TERR054a (keycol INTEGER PRIMARY KEY, datacol VARCHAR SKIP '1,2');

-- SKIPCONDITION on a non-key column
CREATE TABLE TERR054b (keycol INTEGER PRIMARY KEY, datacol VARCHAR SKIPCONDITION "keys(""keycol"")=1");

