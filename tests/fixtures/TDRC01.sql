#################################################################
#								#
# Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
-- The following query is just to check that the \d command works fine between regular queries
select n1.firstname from names n1 limit 2;

--Test \d command with semicolon works fine
\d;

--Test \d command with semicolon works fine
\d tablename;

-- The following query is just to check that the \d command works fine between regular queries
select n1.firstname from names n1 limit 2;

--Test that the \d command without semicolon works fine
\d

--Test that the \d command without semicolon works fine
\d tablename
