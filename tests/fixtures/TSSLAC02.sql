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

-- Below is a simpler version of a Squirrel SQL query found in TSSLAC01.sql
-- The difference is that this one produces some output so at least we can validate the output.
-- Below does an ORDER BY using a column from the pg_catalog.pg_class table

SELECT   *
FROM     (
                   SELECT    n.nspname,
                             c.relname,
                             row_number() AS attnum
                   FROM      pg_catalog.pg_namespace n
                   JOIN      pg_catalog.pg_class c
                   ON        (c.relnamespace = n.oid)) c
ORDER BY nspname,
         relname desc

