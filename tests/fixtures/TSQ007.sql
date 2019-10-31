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

SELECT n.nspname                              AS "Schema",
       c.relname                              AS "Name",
       CASE c.relkind
         WHEN 'r' THEN 'table'
         WHEN 'v' THEN 'view'
         WHEN 'm' THEN 'materialized view'
         WHEN 'i' THEN 'index'
         WHEN 'S' THEN 'sequence'
         WHEN 's' THEN 'special'
         WHEN 'f' THEN 'foreign table'
         WHEN 'p' THEN 'table'
         WHEN 'I' THEN 'index'
       end                                    AS "Type",
       pg_catalog.Pg_get_userbyid(c.relowner) AS "Owner"
FROM   pg_catalog.pg_class c
       LEFT JOIN pg_catalog.pg_namespace n
              ON n.oid = c.relnamespace
WHERE  c.relkind IN ( 'r', 'p', 'v', 'm',
                      'S', 'f', '' )
       AND n.nspname <> 'pg_catalog'
       AND n.nspname <> 'information_schema'
       AND n.nspname !~ '^pg_toast'
       AND pg_catalog.Pg_table_is_visible(c.oid)
ORDER  BY 1,
          2;

