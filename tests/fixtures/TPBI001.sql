#################################################################
#								#
# Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TPBI001 : PowerBI : Startup queries

select
	n.nspname,
	c.relname,
	a.attname,
	a.atttypid,
	t.typname,
	a.attnum,
	a.attlen,
	a.atttypmod,
	a.attnotnull,
	c.relhasrules,
	c.relkind,
	c.oid,
	pg_get_expr(d.adbin, d.adrelid),
	case t.typtype when 'd' then t.typbasetype else 0 end,
	t.typtypmod,
	c.relhasoids,
	'',
	c.relhassubclass
from (((pg_catalog.pg_class c inner join pg_catalog.pg_namespace n on n.oid = c.relnamespace and c.relname like 'NAMES' and n.nspname like 'public')
		inner join pg_catalog.pg_attribute a on (not a.attisdropped) and a.attnum > 0 and a.attrelid = c.oid)
	inner join pg_catalog.pg_type t on t.oid = a.atttypid)
	left outer join pg_attrdef d on a.atthasdef and d.adrelid = a.attrelid and d.adnum = a.attnum
order by n.nspname, c.relname, attnum;

select
	relname,
	nspname,
	relkind
from
	pg_catalog.pg_class c,
	pg_catalog.pg_namespace n
where
	relkind in ('r', 'v', 'm', 'f', 'p') and
	nspname not in ('pg_catalog', 'information_schema', 'pg_toast', 'pg_temp_1') and
	n.oid = relnamespace
order by nspname, relname;

select
	ta.attname,
	ia.attnum,
	ic.relname,
	n.nspname,
	tc.relname
from
	pg_catalog.pg_attribute ta,
	pg_catalog.pg_attribute ia,
	pg_catalog.pg_class tc,
	pg_catalog.pg_index i,
	pg_catalog.pg_namespace n,
	pg_catalog.pg_class ic
where
	tc.relname = 'NAMES' AND
	n.nspname = 'public' AND
	tc.oid = i.indrelid AND
	n.oid = tc.relnamespace AND
	i.indisprimary = 't' AND
	ia.attrelid = i.indexrelid AND
	ta.attrelid = i.indrelid AND
	ta.attnum = i.indkey[ia.attnum-1] AND
	(NOT ta.attisdropped) AND
	(NOT ia.attisdropped) AND
	ic.oid = i.indexrelid
order by ia.attnum;

select	'octo'::name as "PKTABLE_CAT",
	n2.nspname as "PKTABLE_SCHEM",
	c2.relname as "PKTABLE_NAME",
	a2.attname as "PKCOLUMN_NAME",
	'octo'::name as "FKTABLE_CAT",
	n1.nspname as "FKTABLE_SCHEM",
	c1.relname as "FKTABLE_NAME",
	a1.attname as "FKCOLUMN_NAME",
	i::int2 as "KEY_SEQ",
	case ref.confupdtype
		when 'c' then 0::int2
		when 'n' then 2::int2
		when 'd' then 4::int2
		when 'r' then 1::int2
		else 3::int2
	end as "UPDATE_RULE",
	case ref.confdeltype
		when 'c' then 0::int2
		when 'n' then 2::int2
		when 'd' then 4::int2
		when 'r' then 1::int2
		else 3::int2
	end as "DELETE_RULE",
	ref.conname as "FK_NAME",
	cn.conname as "PK_NAME",
	case
		when ref.condeferrable then
			case
			when ref.condeferred then 5::int2
			else 6::int2
			end
		else 7::int2
	end as "DEFERRABILITY"
 from
 ((((((( (select cn.oid, conrelid, conkey, confrelid, confkey,
	 generate_series(array_lower(conkey, 1), array_upper(conkey, 1)) as i,
	 confupdtype, confdeltype, conname,
	 condeferrable, condeferred
  from pg_catalog.pg_constraint cn,
	pg_catalog.pg_class c,
	pg_catalog.pg_namespace n
  where contype = 'f'
   and  conrelid = c.oid
   and  relname = 'NAMES'
   and  n.oid = c.relnamespace
   and  n.nspname = 'public'
 ) ref
 inner join pg_catalog.pg_class c1
  on c1.oid = ref.conrelid)
 inner join pg_catalog.pg_namespace n1
  on  n1.oid = c1.relnamespace)
 inner join pg_catalog.pg_attribute a1
  on  a1.attrelid = c1.oid
  and  a1.attnum = conkey[i])
 inner join pg_catalog.pg_class c2
  on  c2.oid = ref.confrelid)
 inner join pg_catalog.pg_namespace n2
  on  n2.oid = c2.relnamespace)
 inner join pg_catalog.pg_attribute a2
  on  a2.attrelid = c2.oid
  and  a2.attnum = confkey[i])
 left outer join pg_catalog.pg_constraint cn
  on cn.conrelid = ref.confrelid
  and cn.contype = 'p')
  order by ref.oid, ref.i;

select "EMPLOYEEID",
    "LASTNAME",
    "FIRSTNAME",
    "BIRTHDATE",
    "PHOTO",
    "NOTES"
from "octo"."public"."EMPLOYEES";
