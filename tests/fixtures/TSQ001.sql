#################################################################
#								#
# Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

SELECT t.typname,t.oid
FROM pg_catalog.pg_type t
  JOIN pg_catalog.pg_namespace n
  ON (t.typnamespace = n.oid)
WHERE n.nspname  != 'pg_toast'
  AND (t.typrelid = 0 OR
    (SELECT c.relkind = 'c' FROM pg_catalog.pg_class c WHERE c.oid = t.typrelid));

