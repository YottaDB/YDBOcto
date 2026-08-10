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

-- TPBI005 : psqlodbc >= REL-18_00_0002 : SQLPrimaryKeys selects pg_index.indnkeyatts

-- The column resolves on its own. Before it was added to pg_catalog.pg_index it raised
-- ERR_UNKNOWN_COLUMN_NAME. pg_index is never populated, so an empty result is expected.
select indnkeyatts from pg_catalog.pg_index;

-- indnkeyatts is indnatts minus any INCLUDE (covering) columns. Octo has no INCLUDE indexes, so the
-- two are always equal here and this predicate is never restrictive.
select indnatts,indnkeyatts from pg_catalog.pg_index where indnkeyatts <= indnatts;

-- The SQLPrimaryKeys query as emitted by psqlodbc >= REL-18_00_0002 (info.c, PGAPI_PrimaryKeys, the
-- qno == 1 path). This is the same query already present in TPBI001.sql, with the two changes psqlodbc
-- has made to it since: every operator is schema qualified (REL-17_00_0007, YDBOcto#1141) and the new
-- "ia.attnum <= i.indnkeyatts" predicate is appended (REL-18_00_0002, PR #171). Line breaks added for
-- readability; psqlodbc emits it as a single line.
-- IMPORTANT: this returns 0 rows, and the TPBI001.sql form of it returned 0 rows before this change
-- too. Nothing in Octo ever populates pg_catalog.pg_index -- it is a schema-only table. So this subtest
-- records only that the query now runs to completion instead of failing with ERR_UNKNOWN_COLUMN_NAME.
-- It does NOT assert that SQLPrimaryKeys reports primary keys. Making it do so requires populating
-- pg_index at CREATE TABLE time, and real array support for indkey so that the
-- "ta.attnum = i.indkey[ia.attnum-1]" join works for multi-column primary keys.
-- The "arrays" warning comes from that indkey subscript and is expected. It is a warning, not an
-- error: the query completes.
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
	tc.relname = 'names' AND
	n.nspname = 'public' AND
	tc.oid operator(pg_catalog.=) i.indrelid AND
	n.oid operator(pg_catalog.=) tc.relnamespace AND
	i.indisprimary operator(pg_catalog.=) 't' AND
	ia.attrelid operator(pg_catalog.=) i.indexrelid AND
	ta.attrelid operator(pg_catalog.=) i.indrelid AND
	ta.attnum operator(pg_catalog.=) i.indkey[ia.attnum-1] AND
	(NOT ta.attisdropped) AND
	(NOT ia.attisdropped) AND
	ic.oid operator(pg_catalog.=) i.indexrelid AND
	ia.attnum operator(pg_catalog.<=) i.indnkeyatts
order by ia.attnum;
