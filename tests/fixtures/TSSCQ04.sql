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

SELECT t.typlen
FROM   pg_catalog.pg_type t,
       pg_catalog.pg_namespace n
WHERE  t.typnamespace = n.oid
       AND t.typname = 'name'
       AND n.nspname = 'pg_catalog'
