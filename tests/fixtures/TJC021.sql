#################################################################
#								#
# Copyright (c) 2023-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TJC021 : OCTO960 : Squirrel SQL and/or SQL Workbench queries using PostgreSQL JDBC driver 42.6.0

-- ################################################################################
-- Test of "::regclass" and "REGCLASS" function.
-- ################################################################################

-- Test that NULL::regclass does not issue an error.
select NULL::regclass;
select regclass(NULL);

-- Test that 1::regclass issues ERR_UNKNOWN_FUNCTION error.
select 1::regclass;
select regclass(1);

-- Test that 1.1::regclass issues ERR_UNKNOWN_FUNCTION error.
select 1.1::regclass;
select regclass(1.1);

-- Test that true::regclass issues ERR_UNKNOWN_FUNCTION error.
select true::regclass;
select regclass(true);

-- Test that `::regclass` works if a schema.tablename syntax is used as well.
-- Also test that if table or schema name is surrounded by double-quotes it still works.
-- Also test that if schema name is in lower case, it still works as long as table name is in upper case.
select 'PG_CLASS'::regclass;
select 'pg_catalog."PG_CLASS"'::regclass;
select '"PG_CATALOG"."PG_CLASS"'::regclass;
select '"pg_catalog"."PG_CLASS"'::regclass;
select 'NAMES'::regclass;
select 'public.NAMES'::regclass;
select '"public"."NAMES"'::regclass;

-- Test that if table name is not in proper case, an ERR_UNKNOWN_TABLE_OR_VIEW error is issued.
select 'pg_class'::regclass;
select 'pg_catalog."pg_class"'::regclass;
select '"PG_CATALOG"."pg_class"'::regclass;
select '"pg_catalog"."pg_class"'::regclass;
select 'names'::regclass;
select 'public.names'::regclass;
select '"public"."names"'::regclass;

-- Test that queries from SquirrelSQL using "::regclass" do not issue an error
SELECT TRUE FROM   pg_attribute WHERE  attrelid = '"public"."PG_CATALOG.PG_NAMESPACE"'::regclass AND    attname = 'oid' AND NOT attisdropped;
SELECT TRUE FROM   pg_attribute WHERE  attrelid = '"public"."NAMES"'::regclass AND    attname = 'oid' AND NOT attisdropped;
SELECT NULL AS TABLE_CAT, n.nspname AS TABLE_SCHEM, c.relname AS TABLE_NAME,  CASE n.nspname ~ '^pg_' OR n.nspname = 'information_schema'  WHEN true THEN CASE  WHEN n.nspname = 'pg_catalog' OR n.nspname = 'information_schema' THEN CASE c.relkind   WHEN 'r' THEN 'SYSTEM TABLE'   WHEN 'v' THEN 'SYSTEM VIEW'   WHEN 'i' THEN 'SYSTEM INDEX'   ELSE NULL   END  WHEN n.nspname = 'pg_toast' THEN CASE c.relkind   WHEN 'r' THEN 'SYSTEM TOAST TABLE'   WHEN 'i' THEN 'SYSTEM TOAST INDEX'   ELSE NULL   END  ELSE CASE c.relkind   WHEN 'r' THEN 'TEMPORARY TABLE'   WHEN 'p' THEN 'TEMPORARY TABLE'   WHEN 'i' THEN 'TEMPORARY INDEX'   WHEN 'S' THEN 'TEMPORARY SEQUENCE'   WHEN 'v' THEN 'TEMPORARY VIEW'   ELSE NULL   END  END  WHEN false THEN CASE c.relkind  WHEN 'r' THEN 'TABLE'  WHEN 'p' THEN 'PARTITIONED TABLE'  WHEN 'i' THEN 'INDEX'  WHEN 'P' then 'PARTITIONED INDEX'  WHEN 'S' THEN 'SEQUENCE'  WHEN 'v' THEN 'VIEW'  WHEN 'c' THEN 'TYPE'  WHEN 'f' THEN 'FOREIGN TABLE'  WHEN 'm' THEN 'MATERIALIZED VIEW'  ELSE NULL  END  ELSE NULL  END  AS TABLE_TYPE, d.description AS REMARKS,  '' as TYPE_CAT, '' as TYPE_SCHEM, '' as TYPE_NAME, '' AS SELF_REFERENCING_COL_NAME, '' AS REF_GENERATION  FROM pg_catalog.pg_namespace n, pg_catalog.pg_class c  LEFT JOIN pg_catalog.pg_description d ON (c.oid = d.objoid AND d.objsubid = 0  and d.classoid = 'pg_class'::regclass)  WHERE c.relnamespace = n.oid  AND (false  OR ( c.relkind = 'r' AND n.nspname !~ '^pg_' AND n.nspname <> 'information_schema' )  OR ( c.relkind = 'r' AND (n.nspname = 'pg_catalog' OR n.nspname = 'information_schema') )  OR ( c.relkind = 'v' AND n.nspname <> 'pg_catalog' AND n.nspname <> 'information_schema' ) )  ORDER BY TABLE_TYPE,TABLE_SCHEM,TABLE_NAME;

-- Test that queries from SquirrelSQL that use "p.prokind='p'" do not issue `ERR_TYPE_MISMATCH` error
SELECT NULL AS PROCEDURE_CAT, n.nspname AS PROCEDURE_SCHEM, p.proname AS PROCEDURE_NAME, NULL, NULL, NULL, d.description AS REMARKS, 2 AS PROCEDURE_TYPE,  p.proname || '_' || p.oid AS SPECIFIC_NAME  FROM pg_catalog.pg_namespace n, pg_catalog.pg_proc p  LEFT JOIN pg_catalog.pg_description d ON (p.oid=d.objoid)  LEFT JOIN pg_catalog.pg_class c ON (d.classoid=c.oid AND c.relname='pg_proc')  LEFT JOIN pg_catalog.pg_namespace pn ON (c.relnamespace=pn.oid AND pn.nspname='pg_catalog')  WHERE p.pronamespace=n.oid  AND p.prokind='p' ORDER BY PROCEDURE_SCHEM, PROCEDURE_NAME, p.oid::text;

-- Test query from SquirrelSQL using `pg_attribute.attgenerated` column does not issue an error
SELECT * FROM (SELECT n.nspname,c.relname,a.attname,a.atttypid,a.attnotnull OR (t.typtype = 'd' AND t.typnotnull) AS attnotnull,a.atttypmod,a.attlen,t.typtypmod,row_number() OVER (PARTITION BY a.attrelid ORDER BY a.attnum) AS attnum, nullif(a.attidentity, '') as attidentity,nullif(a.attgenerated, '') as attgenerated,pg_catalog.pg_get_expr(def.adbin, def.adrelid) AS adsrc,dsc.description,t.typbasetype,t.typtype  FROM pg_catalog.pg_namespace n  JOIN pg_catalog.pg_class c ON (c.relnamespace = n.oid)  JOIN pg_catalog.pg_attribute a ON (a.attrelid=c.oid)  JOIN pg_catalog.pg_type t ON (a.atttypid = t.oid)  LEFT JOIN pg_catalog.pg_attrdef def ON (a.attrelid=def.adrelid AND a.attnum = def.adnum)  LEFT JOIN pg_catalog.pg_description dsc ON (c.oid=dsc.objoid AND a.attnum = dsc.objsubid)  LEFT JOIN pg_catalog.pg_class dc ON (dc.oid=dsc.classoid AND dc.relname='pg_class')  LEFT JOIN pg_catalog.pg_namespace dn ON (dc.relnamespace=dn.oid AND dn.nspname='pg_catalog')  WHERE c.relkind in ('r','p','v','f','m') and a.attnum > 0 AND NOT a.attisdropped  AND n.nspname LIKE 'public' AND c.relname LIKE 'NAMES') c WHERE true  AND attname LIKE '%' ORDER BY nspname,c.relname,attnum;

-- Test that queries from SquirrelSQL using "::regproc" do not issue an error
SELECT typinput='pg_catalog.array_in'::regproc as is_array, typtype, typname, pg_type.oid   FROM pg_catalog.pg_type   LEFT JOIN (select ns.oid as nspoid, ns.nspname, r.r           from pg_namespace as ns           join ( select s.r, (current_schemas(false))[s.r] as nspname                    from generate_series(1, array_upper(current_schemas(false), 1)) as s(r) ) as r          using ( nspname )        ) as sp     ON sp.nspoid = typnamespace  WHERE pg_type.oid = $1  ORDER BY sp.r, pg_type.oid DESC;

