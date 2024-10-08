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

drop table if exists tdtt067order1;
create table tdtt067order1 (id int primary key,dob date,dot time, dots time with time zone, dotz timestamp, dotzts timestamp with time zone);
-- Original query has c.oid after c.relkind in select list but this value can be different in Postgres and Octo se we skip its inclusion here
-- Original query has attidentity between `0,.,c.relhassubclass` in the below query but its value is 1 in Octo and Null in Postgres so leaving it out here
-- Original query has pg_get_expr(d.adbin, d.adrelid) in the blow query select list but it was seen to have `null` in Postgres output when jdbc driver is selected and empty string in case of Octo so this is removed from the query
select n.nspname, c.relname, a.attname, a.atttypid, t.typname, a.attnum, a.attlen, a.atttypmod, a.attnotnull, c.relhasrules, c.relkind, case t.typtype when 'd' then t.typbasetype else 0 end, t.typtypmod, 0, c.relhassubclass from (((pg_catalog.pg_class c inner join pg_catalog.pg_namespace n on n.oid = c.relnamespace and c.relname like 'tdtt067order1' and n.nspname like 'public') inner join pg_catalog.pg_attribute a on (not a.attisdropped) and a.attnum > 0 and a.attrelid = c.oid) inner join pg_catalog.pg_type t on t.oid = a.atttypid) left outer join pg_attrdef d on a.atthasdef and d.adrelid = a.attrelid and d.adnum = a.attnum order by n.nspname, c.relname, attnum;
drop table tdtt067order1;
