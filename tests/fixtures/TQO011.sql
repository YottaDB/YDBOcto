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

-- TQO011 : #1141 : "operator" is NOT a reserved word and remains usable as an identifier
-- Teaching the parser the OPERATOR() syntax must not cost users the ability to name a column
-- "operator". Postgres allows it unquoted too, so this is crosschecked rather than pinned to a
-- reference file. The third query below uses the identifier and the operator syntax side by side.

drop table if exists tqo011;
create table tqo011 (id integer primary key, operator varchar(10));
insert into tqo011 values (1,'plus');
insert into tqo011 values (2,'minus');
select operator from tqo011;
select operator from tqo011 where operator operator(pg_catalog.=) 'plus';
select tqo011.operator from tqo011 where tqo011.operator operator(pg_catalog.<>) 'plus';
select id as operator from tqo011 where id operator(pg_catalog.<=) 1;
select operator from tqo011 order by operator;
drop table tqo011;
