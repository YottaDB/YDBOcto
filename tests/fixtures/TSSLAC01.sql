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

SELECT   *
FROM     (
                   SELECT    n.nspname,
                             c.relname,
                             a.attname,
                             a.atttypid,
                             a.attnotnull
                   or        (
                                       t.typtype = 'd'
                             AND       t.typnotnull) AS attnotnull,
                             a.atttypmod,
                             a.attlen,
                             row_number() OVER (partition BY a.attrelid ORDER BY a.attnum) AS attnum,
                             NULL                                                          AS attidentity,
                             pg_catalog.pg_get_expr(def.adbin, def.adrelid)                AS adsrc,
                             dsc.description,
                             t.typbasetype,
                             t.typtype
                   FROM      pg_catalog.pg_namespace n
                   JOIN      pg_catalog.pg_class c
                   ON        (
                                       c.relnamespace = n.oid)
                   JOIN      pg_catalog.pg_attribute a
                   ON        (
                                       a.attrelid=c.oid)
                   JOIN      pg_catalog.pg_type t
                   ON        (
                                       a.atttypid = t.oid)
                   LEFT JOIN pg_catalog.pg_attrdef def
                   ON        (
                                       a.attrelid=def.adrelid
                             AND       a.attnum = def.adnum)
                   LEFT JOIN pg_catalog.pg_description dsc
                   ON        (
                                       c.oid=dsc.objoid
                             AND       a.attnum = dsc.objsubid)
                   LEFT JOIN pg_catalog.pg_class dc
                   ON        (
                                       dc.oid=dsc.classoid
                             AND       dc.relname='pg_class')
                   LEFT JOIN pg_catalog.pg_namespace dn
                   ON        (
                                       dc.relnamespace=dn.oid
                             AND       dn.nspname='pg_catalog')
                   WHERE     c.relkind IN ('r',
                                           'p',
                                           'v',
                                           'f',
                                           'm')
                   AND       a.attnum > 0
                   AND       NOT a.attisdropped
                   AND       n.nspname LIKE 'public'
                   AND       c.relname LIKE 'names') c
WHERE    true
AND      attname LIKE '%'
ORDER BY nspname,
         c.relname,
         attnum
