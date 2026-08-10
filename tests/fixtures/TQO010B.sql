#################################################################
#								#
# Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TQO010B : #1141 : The SQLColumns query issued by psqlodbc >= REL-17_00_0007 returns the same rows as Postgres
-- Same query as TQO010A.sql, crosschecked against Postgres. As in TDTT067.sql, a few columns of the original query
-- have to be left out of the select list because they legitimately differ between Octo and Postgres:
--	a) "c.oid"                        : the OID counters of the two are unrelated.
--	b) "attidentity"                  : 1 in Octo, NULL in Postgres.
--	c) "pg_get_expr(d.adbin, d.adrelid)" : NULL in Postgres, empty string in Octo.

drop table if exists tqo010_location;
create table tqo010_location (id integer primary key, city varchar(30), zipcode integer);
select n.nspname, c.relname, a.attname, a.atttypid, t.typname, a.attnum, a.attlen, a.atttypmod, a.attnotnull, c.relhasrules, c.relkind, case t.typtype when 'd' then t.typbasetype else 0 end, t.typtypmod, 0, c.relhassubclass from (((pg_catalog.pg_class c inner join pg_catalog.pg_namespace n on n.oid = c.relnamespace and c.relname like 'tqo010\_location' and n.nspname like 'public') inner join pg_catalog.pg_attribute a on (not a.attisdropped) and a.attnum operator(pg_catalog.>) 0 and a.attrelid operator(pg_catalog.=) c.oid) inner join pg_catalog.pg_type t on t.oid operator(pg_catalog.=) a.atttypid) left outer join pg_attrdef d on a.atthasdef and d.adrelid operator(pg_catalog.=) a.attrelid and d.adnum operator(pg_catalog.=) a.attnum order by n.nspname, c.relname, attnum;
drop table tqo010_location;
