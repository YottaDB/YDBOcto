-- ######################################################################
-- #									#
-- # Copyright (c) 2020-2026 YottaDB LLC and/or its subsidiaries.	#
-- # All rights reserved.						#
-- #									#
-- #	This source code contains the intellectual property		#
-- #	of its copyright holder(s), and is made available		#
-- #	under a license.  If you do not know the terms of		#
-- #	the license, please stop and do not read further.		#
-- #									#
-- ######################################################################

-- An UPDATE in Postgres processes rows in the order they are stored in the table whereas Octo processes them in
-- primary key order. Since UNIQUE constraints are checked as each row is updated, a multi-row UPDATE can then succeed
-- in one and fail in the other. The query generator therefore calls this function after every INSERT, UPDATE and
-- DELETE to rewrite the table in primary key order.
-- This is a CREATE OR REPLACE and is placed first because "load_postgres_fixture" stops at the first error, which
-- is the CREATE FUNCTION of SAMEVALUE below if this file was previously loaded into this database.
CREATE OR REPLACE FUNCTION clusterOnPrimaryKey(tableName text) RETURNS void AS $$
DECLARE
	pkeyIndex regclass;
BEGIN
	SELECT indexrelid::regclass INTO pkeyIndex FROM pg_index WHERE indrelid = tableName::regclass AND indisprimary;
	IF pkeyIndex IS NOT NULL THEN
		EXECUTE format('CLUSTER %s USING %s', tableName::regclass, pkeyIndex);
	END IF;
END
$$ LANGUAGE plpgsql;

CREATE FUNCTION SAMEVALUE(integer) RETURNS integer
    AS 'select $1;'
    LANGUAGE SQL
    IMMUTABLE;
CREATE FUNCTION SAMEVALUE(numeric) RETURNS numeric
    AS 'select $1;'
    LANGUAGE SQL
    IMMUTABLE;
CREATE FUNCTION SAMEVALUE(varchar) RETURNS varchar
    AS 'select $1;'
    LANGUAGE SQL
    IMMUTABLE;
CREATE FUNCTION SAMEVALUE(boolean) RETURNS boolean
    AS 'select $1;'
    LANGUAGE SQL
    IMMUTABLE;
CREATE FUNCTION SAMEVALUE(date) RETURNS date
    AS 'select $1;'
    LANGUAGE SQL
    IMMUTABLE;
