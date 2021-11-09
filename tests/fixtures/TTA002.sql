#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TTA002 : OCTO759 : COUNT(DISTINCT TABLENAME.ASTERISK) produces incorrect results in some cases : names database

-- Below are simpler queries (than noted in TTA001.sql) that demonstrate the same YDBOcto#759 issue.
-- Some of them are included more for completeness of testing even though they passed without the YDBOcto#759 fixes.

-- Below is from a query noted at https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/759#note_728237002
SELECT COUNT(DISTINCT n1.*) FROM (SELECT 'Joey' UNION SELECT NULL) n1;

-- Below are queries noted at https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/759#note_729525246
SELECT COUNT(DISTINCT n1.*) FROM (VALUES ('Joey'), (NULL)) n1;
SELECT COUNT(DISTINCT n1.*) FROM (VALUES ('Joey', 'dummy'), (NULL, 'dummy')) n1;

-- Below are queries that use the names database and demonstrate similar issues with COUNT(DISTINCT TABLENAME.ASTERISK)
SELECT COUNT(DISTINCT n1.*) FROM (SELECT lastname from names) n1;

