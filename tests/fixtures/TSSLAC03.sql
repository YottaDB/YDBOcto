#################################################################
#								#
# Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- Below is a slightly revised form of the query found in TSSLAC02.sql.
-- This query has one more column from a sub-query in the ORDER BY of the outer query.
-- Like TSSLAC02.sql, this query too produces some output so we can validate the output.

SELECT   *
FROM     (
                   SELECT    n.nspname,
                             c.relname,
                             row_number() AS attnum
                   FROM      pg_catalog.pg_namespace n
                   JOIN      pg_catalog.pg_class c
                   ON        (c.relnamespace = n.oid)) c
ORDER BY nspname,
         relname desc,
	 attnum

