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

SELECT NULL          AS PROCEDURE_CAT,
       n.nspname     AS PROCEDURE_SCHEM,
       p.proname     AS PROCEDURE_NAME,
       NULL,
       NULL,
       NULL,
       d.description AS REMARKS,
       2             AS PROCEDURE_TYPE,
       p.proname
       || '_'
       || p.oid      AS SPECIFIC_NAME
FROM   pg_catalog.pg_namespace n,
       pg_catalog.pg_proc p
       left join pg_catalog.pg_description d
              ON ( p.oid = d.objoid )
       left join pg_catalog.pg_class c
              ON ( d.classoid = c.oid
                   AND c.relname = 'pg_proc' )
       left join pg_catalog.pg_namespace pn
              ON ( c.relnamespace = pn.oid
                   AND pn.nspname = 'pg_catalog' )
WHERE  p.pronamespace = n.oid
ORDER  BY procedure_schem,
          procedure_name,
          p.oid :: text

