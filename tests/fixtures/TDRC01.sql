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
\d names;

-- The following query is just to check that the \d command works fine between regular queries
select n1.firstname from names n1 limit 2;

--Test that the \d tablename without semicolon works fine
\d names

-- Test that the \d command without semicolon issues error which is evident when the keyword 'select'
-- from the following query ("select 1;") gets treated as the table name and gets a confused parse output.
\d

select 1;

-- Having the below query as the last query in this file used to fail with the below assert in an interim version of the
-- code and hence is added to this test.
--      octo: YDBOcto/src/run_query.c:235: run_query: Assertion `table_alias_STATEMENT == result->type' failed.
\d
