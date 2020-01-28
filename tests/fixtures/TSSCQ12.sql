#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

SELECT   nspname AS table_schem,
         NULL    AS table_catalog
FROM     pg_catalog.pg_namespace
WHERE    nspname <> 'pg_toast'
AND      (
                  nspname !~ '^pg_temp_'
         OR       nspname = (pg_catalog.Current_schemas(true))[1])
AND      (
                  nspname !~ '^pg_toast_temp_'
         OR       nspname = replace((pg_catalog.current_schemas(true))[1], 'pg_temp_', 'pg_toast_temp_'))
ORDER BY table_schem
