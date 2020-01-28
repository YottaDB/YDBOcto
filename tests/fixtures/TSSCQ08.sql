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

SELECT c.oid,
       a.attnum,
       a.attname,
       c.relname,
       n.nspname,
       a.attnotnull
        OR ( t.typtype = 'd'
             AND t.typnotnull ),
       a.attidentity != ''
        OR pg_catalog.Pg_get_expr(d.adbin, d.adrelid) LIKE '%nextval(%'
FROM   pg_catalog.pg_class c
       JOIN pg_catalog.pg_namespace n
         ON ( c.relnamespace = n.oid )
       JOIN pg_catalog.pg_attribute a
         ON ( c.oid = a.attrelid )
       JOIN pg_catalog.pg_type t
         ON ( a.atttypid = t.oid )
       LEFT JOIN pg_catalog.pg_attrdef d
              ON ( d.adrelid = a.attrelid
                   AND d.adnum = a.attnum )
       JOIN (SELECT 2615 AS oid,
                    1    AS attnum) vals
         ON ( c.oid = vals.oid
              AND a.attnum = vals.attnum )
