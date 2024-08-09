#################################################################
#								#
# Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

------------------------------------------------------------------------------------------------------------------------
-- The below is needed for YDBOcto#929 to convert function/table names that were previously in upper-case to lower-case
------------------------------------------------------------------------------------------------------------------------
DROP FUNCTION IF EXISTS "ABS"(NUMERIC);
DROP FUNCTION IF EXISTS "ABS"(INTEGER);
DROP FUNCTION IF EXISTS "CONCAT"(VARCHAR, VARCHAR);
DROP FUNCTION IF EXISTS "CONCAT"(VARCHAR, VARCHAR, VARCHAR);
DROP FUNCTION IF EXISTS "ROUND"(NUMERIC, INTEGER);
DROP FUNCTION IF EXISTS "ROUND"(INTEGER, INTEGER);
DROP FUNCTION IF EXISTS "TRUNC"(NUMERIC, INTEGER);
DROP FUNCTION IF EXISTS "TRUNC"(INTEGER, NUMERIC);
DROP FUNCTION IF EXISTS "TRUNC"(NUMERIC, NUMERIC);
DROP FUNCTION IF EXISTS "TRUNC"(INTEGER, INTEGER);
DROP FUNCTION IF EXISTS "TRUNCATE"(NUMERIC, INTEGER);
DROP FUNCTION IF EXISTS "TRUNCATE"(INTEGER, NUMERIC);
DROP FUNCTION IF EXISTS "TRUNCATE"(NUMERIC, NUMERIC);
DROP FUNCTION IF EXISTS "TRUNCATE"(INTEGER, INTEGER);
DROP FUNCTION IF EXISTS "NOW"();
DROP FUNCTION IF EXISTS "DAY"(VARCHAR);
DROP FUNCTION IF EXISTS "LOCALTIME"();
DROP FUNCTION IF EXISTS "LOCALTIMESTAMP"();
DROP FUNCTION IF EXISTS "CURRENT_TIMESTAMP"();
DROP FUNCTION IF EXISTS "CURRENT_TIME"();
DROP FUNCTION IF EXISTS "DATE_FORMAT"(VARCHAR, VARCHAR);
DROP FUNCTION IF EXISTS "DAYOFMONTH"(VARCHAR);
DROP FUNCTION IF EXISTS "LPAD"(VARCHAR, INTEGER);
DROP FUNCTION IF EXISTS "LPAD"(VARCHAR, INTEGER, VARCHAR);
DROP FUNCTION IF EXISTS "REPLACE"(VARCHAR, VARCHAR, VARCHAR);
DROP FUNCTION IF EXISTS "ROW_NUMBER"();
DROP FUNCTION IF EXISTS "SET_CONFIG"(VARCHAR, VARCHAR, BOOLEAN);
DROP FUNCTION IF EXISTS "VERSION"();
DROP FUNCTION IF EXISTS "PG_CATALOG.PG_TABLE_IS_VISIBLE"(INTEGER);
DROP FUNCTION IF EXISTS "PG_CATALOG.PG_GET_USERBYID"(INTEGER);
DROP FUNCTION IF EXISTS "CURRENT_SCHEMA"();
DROP FUNCTION IF EXISTS "CURRENT_DATABASE"();
DROP FUNCTION IF EXISTS "CURRENT_CATALOG"();
DROP FUNCTION IF EXISTS "CURRENT_ROLE"();
DROP FUNCTION IF EXISTS "CURRENT_USER"();
DROP FUNCTION IF EXISTS "SESSION_USER"();
DROP FUNCTION IF EXISTS "USER"();
DROP FUNCTION IF EXISTS "PG_CATALOG.CURRENT_SCHEMAS"(BOOLEAN);
DROP FUNCTION IF EXISTS "PG_CATALOG.OBJ_DESCRIPTION"(INTEGER, VARCHAR);
DROP FUNCTION IF EXISTS "PG_CATALOG.PG_BACKEND_PID"();
DROP FUNCTION IF EXISTS "PG_CATALOG.PG_GET_EXPR"(VARCHAR, INTEGER);
DROP FUNCTION IF EXISTS "PG_CATALOG.PG_GET_EXPR"(VARCHAR, INTEGER, BOOLEAN);
DROP FUNCTION IF EXISTS "PG_ENCODING_TO_CHAR"(INTEGER);
DROP FUNCTION IF EXISTS "PG_IS_IN_RECOVERY"();
DROP FUNCTION IF EXISTS "PG_IS_XLOG_REPLAY_PAUSED"();
DROP FUNCTION IF EXISTS "HAS_DATABASE_PRIVILEGE"(INTEGER, VARCHAR);
DROP FUNCTION IF EXISTS "HAS_DATABASE_PRIVILEGE"(VARCHAR, VARCHAR, VARCHAR);
DROP FUNCTION IF EXISTS "ARRAY_LOWER"(INTEGER, INT);
DROP FUNCTION IF EXISTS "ARRAY_LOWER"(NUMERIC, INT);
DROP FUNCTION IF EXISTS "ARRAY_LOWER"(VARCHAR, INT);
DROP FUNCTION IF EXISTS "ARRAY_LOWER"(BOOLEAN, INT);
DROP FUNCTION IF EXISTS "ARRAY_UPPER"(INTEGER, INT);
DROP FUNCTION IF EXISTS "ARRAY_UPPER"(NUMERIC, INT);
DROP FUNCTION IF EXISTS "ARRAY_UPPER"(VARCHAR, INT);
DROP FUNCTION IF EXISTS "ARRAY_UPPER"(BOOLEAN, INT);
DROP FUNCTION IF EXISTS "GENERATE_SERIES"(INT, INT);
DROP FUNCTION IF EXISTS "REGCLASS"(VARCHAR);
DROP FUNCTION IF EXISTS "REGPROC"(VARCHAR);
DROP FUNCTION IF EXISTS "FORMAT_TYPE"(INTEGER, INTEGER);
DROP FUNCTION IF EXISTS "SUBSTRING"(VARCHAR);
DROP FUNCTION IF EXISTS "SUBSTRING"(VARCHAR, INTEGER);
DROP FUNCTION IF EXISTS "SUBSTRING"(VARCHAR, INTEGER, INTEGER);
-- Note: Need to use KEEPDATA for every DROP TABLE command in this file.
-- Search for OPTIONAL_KEEPDATA comment in run_query.c for details.
DROP TABLE IF EXISTS "OCTOONEROWTABLE" KEEPDATA;
DROP TABLE IF EXISTS "PG_CATALOG.PG_NAMESPACE" KEEPDATA;
DROP TABLE IF EXISTS "PG_CATALOG.PG_TYPE" KEEPDATA;
DROP TABLE IF EXISTS "PG_CATALOG.PG_CLASS" KEEPDATA;
DROP TABLE IF EXISTS "PG_CATALOG.PG_DESCRIPTION" KEEPDATA;
DROP TABLE IF EXISTS "INFORMATION_SCHEMA.TABLES" KEEPDATA;
DROP TABLE IF EXISTS "PG_CATALOG.PG_PROC" KEEPDATA;
DROP TABLE IF EXISTS "PG_CATALOG.PG_ATTRIBUTE" KEEPDATA;
DROP TABLE IF EXISTS "PG_CATALOG.PG_ATTRDEF" KEEPDATA;
DROP TABLE IF EXISTS "PG_CATALOG.PG_SETTINGS" KEEPDATA;
DROP TABLE IF EXISTS "PG_CATALOG.PG_DATABASE" KEEPDATA;
DROP TABLE IF EXISTS "PG_CATALOG.PG_ROLES" KEEPDATA;
DROP TABLE IF EXISTS "PG_CATALOG.PG_USER" KEEPDATA;
DROP TABLE IF EXISTS "PG_CATALOG.PG_RANGE" KEEPDATA;
DROP TABLE IF EXISTS "PG_CATALOG.PG_ENUM" KEEPDATA;
DROP TABLE IF EXISTS "PG_CATALOG.PG_INDEX" KEEPDATA;
DROP TABLE IF EXISTS "PG_CATALOG.PG_CONSTRAINT" KEEPDATA;
DROP TABLE IF EXISTS "PG_CATALOG.PG_AM" KEEPDATA;
DROP TABLE IF EXISTS "PG_NAMESPACE" KEEPDATA;
DROP TABLE IF EXISTS "PG_TYPE" KEEPDATA;
DROP TABLE IF EXISTS "PG_CLASS" KEEPDATA;
DROP TABLE IF EXISTS "PG_DESCRIPTION" KEEPDATA;
DROP TABLE IF EXISTS "PG_PROC" KEEPDATA;
DROP TABLE IF EXISTS "PG_ATTRIBUTE" KEEPDATA;
DROP TABLE IF EXISTS "PG_ATTRDEF" KEEPDATA;
DROP TABLE IF EXISTS "PG_SETTINGS" KEEPDATA;
DROP TABLE IF EXISTS "PG_DATABASE" KEEPDATA;
DROP TABLE IF EXISTS "PG_ROLES" KEEPDATA;
DROP TABLE IF EXISTS "PG_USER" KEEPDATA;
DROP TABLE IF EXISTS "PG_RANGE" KEEPDATA;
DROP TABLE IF EXISTS "PG_ENUM" KEEPDATA;
DROP TABLE IF EXISTS "PG_INDEX" KEEPDATA;
DROP TABLE IF EXISTS "PG_CONSTRAINT" KEEPDATA;
DROP TABLE IF EXISTS "PG_AM" KEEPDATA;

/* Has exactly one row, and is used to allow SELECT without a FROM clause
 */
CREATE TABLE octoOneRowTable (id INTEGER primary key) GLOBAL "^%ydboctoocto(""tables"",""octoOneRow"",keys(""id""))" READONLY;

/* Used to store a list of 'namespaces'; basically everything we do should
 * fit in one of: public, pg_catalog, or information_schema
 */
CREATE TABLE pg_catalog.pg_namespace (
  nspname VARCHAR,
  nspowner INTEGER,
  nspacl VARCHAR,
  oid INTEGER primary key
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_namespace"",keys(""oid""))" READONLY;

-- Note: Above GLOBAL is populated in `tests/fixtures/octo-seed.zwr` using the following query
--	select *,oid from pg_catalog.pg_type where typname in ('bool','int4','numeric','varchar','name');
-- And doing the following post-processing
--	a) Removing spaces from the output.
--	b) Replace `t` and `f` occurrences with `1` and `0` respectively.

CREATE TABLE pg_catalog.pg_type (
  typname VARCHAR(25) PRIMARY KEY,
  typnamespace INTEGER,
  typowner INTEGER,
  typlen INTEGER,
  typbyval BOOL,		-- specifying BOOL here just to test that works as good as BOOLEAN
  typtype VARCHAR(25),
  typcategory VARCHAR(25),
  typispreferred BOOLEAN,
  typisdefined BOOLEAN,
  typdelim VARCHAR(25),
  typrelid INTEGER,
  typelem INTEGER,
  typarray INTEGER,
  typinput VARCHAR(25),
  typoutput VARCHAR(25),
  typreceive INTEGER,		-- regproc in Postgres but approximated with INTEGER in Octo
  typsend VARCHAR(25),
  typmodin VARCHAR(25),
  typmodout VARCHAR(25),
  typanalyze VARCHAR(25),
  typalign VARCHAR(25),
  typstorage VARCHAR(25),
  typnotnull BOOLEAN,
  typbasetype INTEGER,
  typtypmod INTEGER,
  typndims INTEGER,
  typcollation INTEGER,
  typdefaultbin VARCHAR(25),
  typdefault VARCHAR(25),
  typacl VARCHAR(25),
  oid INTEGER
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_type"",keys(""typname""))" READONLY;

/* Stores any table-like relations in the database
 */
CREATE TABLE pg_catalog.pg_class (
  relname VARCHAR,
  relnamespace INTEGER,
  reltype INTEGER,
  reloftype INTEGER,
  relowner INTEGER,
  relam INTEGER,
  relfilenode INTEGER,
  reltablespace INTEGER,
  relpages INTEGER,
  reltuples INTEGER,
  relallvisible INTEGER,
  reltoastrelid INTEGER,
  relhasindex BOOLEAN,
  relisshared BOOLEAN,
  relpersistence VARCHAR,
  relkind VARCHAR,
  relnatts INTEGER,
  relchecks INTEGER,
  relhasoids BOOLEAN,
  relhaspkey BOOLEAN,
  relhasrules BOOLEAN,
  relhastriggers BOOLEAN,
  relhassubclass BOOLEAN,
  relrowsecurity BOOLEAN,
  relforcerowsecurity BOOLEAN,
  relispopulated BOOLEAN,
  relreplident VARCHAR,
  relispartition BOOLEAN,
  relfrozenxid INTEGER,
  relminmxid INTEGER,
  relacl VARCHAR,
  reloptions VARCHAR,
  relpartbound VARCHAR,
  oid INTEGER PRIMARY KEY
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_class"",keys(""oid""))" READONLY;

/* Populated via special DDL arguments */
CREATE TABLE pg_catalog.pg_description (
  objoid INTEGER,
  classoid INTEGER,
  objsubid INTEGER,
  description VARCHAR,
  oid INTEGER PRIMARY KEY
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_description"",keys(""oid""))" READONLY;


CREATE TABLE information_schema.tables (
  oid INTEGER primary key,
  table_catalog VARCHAR,
  table_schema VARCHAR,
  table_name VARCHAR,
  table_type VARCHAR,
  self_referencing_column_name VARCHAR,
  reference_generation VARCHAR,
  user_defined_type_catalog VARCHAR,
  user_defined_type_schema VARCHAR,
  user_defined_type_name VARCHAR,
  is_insertable_into VARCHAR,
  is_typed VARCHAR,
  commit_action VARCHAR
) GLOBAL "^%ydboctoocto(""tables"",""information_schema"",""tables"",keys(""oid""))" READONLY;

CREATE TABLE pg_catalog.pg_proc (
  proname VARCHAR,
  pronamespace INTEGER,
  proowner INTEGER,
  prolang INTEGER,
  procost INTEGER,
  prorows INTEGER,
  provariadic INTEGER,
  protransform VARCHAR,
  prokind VARCHAR,
  prosecdef BOOLEAN,
  proleakproof BOOLEAN,
  proisstrict BOOLEAN,
  proretset BOOLEAN,
  provolatile VARCHAR,
  proparallel VARCHAR,
  pronargs INTEGER,
  pronargdefaults INTEGER,
  prorettype INTEGER,
  proargtypes VARCHAR,
  proallargtypes INTEGER,
  proargmodes VARCHAR,
  proargnames VARCHAR,
  proargdefaults VARCHAR,
  protrftypes INTEGER,
  prosrc VARCHAR,
  probin VARCHAR,
  proconfig VARCHAR,
  proacl VARCHAR,
  oid INTEGER primary key
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_proc"",keys(""oid""))" READONLY;

/* Stores column descriptions for tables */
CREATE TABLE pg_catalog.pg_attribute (
  attrelid INTEGER,
  attname VARCHAR,
  atttypid INTEGER,
  attstattarget INTEGER,
  attlen INTEGER,
  attnum INTEGER,
  attndims INTEGER,
  attcacheoff INTEGER,
  atttypmod INTEGER,
  attbyval BOOLEAN,
  attstorage VARCHAR,
  attalign VARCHAR,
  attnotnull BOOLEAN,
  atthasdef BOOLEAN,
  atthasmissing BOOLEAN,
  attgenerated VARCHAR,
  attidentity VARCHAR,
  attisdropped BOOLEAN,
  attislocal BOOLEAN,
  attinhcount INTEGER,
  attcollation INTEGER,
  attacl VARCHAR,
  attoptions VARCHAR,
  attfdwoptions VARCHAR,
  attmissingval VARCHAR,
  oid INTEGER primary key
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_attribute"",keys(""oid""))" READONLY;

/* Stores default values for columns */
CREATE TABLE pg_catalog.pg_attrdef (
 adrelid INTEGER,
 adnum INTEGER,
 adbin VARCHAR,
 adsrc VARCHAR,
 oid INTEGER primary key
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_attrdef"",keys(""oid""))" READONLY;

/* Stores information on run-time parameters. Acts as an alternative interface for SET and SHOW commands
 * Derived from https://www.postgresql.org/docs/11/view-pg-settings.html
 *
 * Note that this table is maintained on a per-session/process basis as it stores runtime parameters.
 * Accordingly, it is stored in a YDB local variable instead of a YDB global variable.
 */
CREATE TABLE pg_catalog.pg_settings (
 name VARCHAR primary key,
 setting VARCHAR,
 unit VARCHAR,
 category VARCHAR,
 short_desc VARCHAR,
 extra_desc VARCHAR,
 context VARCHAR,
 vartype VARCHAR,
 source VARCHAR,
 min_val VARCHAR,
 enumvals VARCHAR,
 boot_val VARCHAR,
 reset_val VARCHAR,
 sourcefile VARCHAR,
 sourceline INTEGER,
 pending_restart BOOLEAN
) GLOBAL "%ydboctoocto(""settings"",""pg_settings"",keys(""name""))";

/* Note that this table definition is just a stub, principally for preventing syntax errors, and so remains
 * to be fully implemented/supported (YDBOcto#588).
 */
CREATE TABLE pg_catalog.pg_database (
 datname VARCHAR,
 datdba INTEGER,
 encoding INTEGER,
 datcollate VARCHAR,
 datctype VARCHAR,
 datistemplate BOOLEAN,
 datallowconn BOOLEAN,
 datconnlimit INTEGER,
 datlastsysoid INTEGER,
 datfrozenxid INTEGER,
 datminmxid INTEGER,
 dattablespace INTEGER,
 datacl INTEGER,
 oid INTEGER primary key
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_database"",keys(""oid""))" READONLY;

/* TODO: This table definition is just a stub, principally for preventing syntax errors, and so remains
 * to be fully implemented/supported per YDBOcto#661.
 */
CREATE TABLE pg_catalog.pg_roles (
	rolname VARCHAR,
	rolsuper BOOLEAN,
	rolinherit BOOLEAN,
	rolcreaterole BOOLEAN,
	rolcreatedb BOOLEAN,
	rolcanlogin BOOLEAN,
	rolreplication BOOLEAN,
	rolconnlimit INTEGER,
	rolpassword VARCHAR,
	rolvaliduntil VARCHAR,
	rolbypassrls BOOLEAN,
	rolconfig VARCHAR,
	oid INTEGER PRIMARY KEY
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_roles"",keys(""oid""))" READONLY;

/* TODO: This table definition is just a stub, principally for preventing syntax errors, and so remains
 * to be fully implemented/supported per YDBOcto#662.
 */
CREATE TABLE pg_catalog.pg_user (
	usename VARCHAR,
	usersysid INTEGER PRIMARY KEY,
	usecreatedb BOOLEAN,
	usesuper BOOLEAN,
	userepl BOOLEAN,
	usebypassrls BOOLEAN,
	passwd VARCHAR,
	valuntil VARCHAR,
	userconfig VARCHAR
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_user"",keys(""usersysid""))" READONLY;

/* Add standard SQL functions and internal Octo functions to the database and catalog
 * Note that some catalog functions do NOT use the types specified for them in PostgreSQL,
 * as Octo doesn't implement them (yet).
 * Note also that some functions are overloaded to account for various valid combinations of
 * parameter types, e.g. accept both `NUMERIC` and `INTEGER` types for the `ABS` function.
 */
CREATE FUNCTION abs(NUMERIC) RETURNS NUMERIC AS $$ABS^%ydboctosqlfunctions;
CREATE FUNCTION abs(INTEGER) RETURNS INTEGER AS $$ABS^%ydboctosqlfunctions;
/* Note that PostgreSQL CONCAT accepts a variable number of arguments. Since Octo doesn't support this,
 * just use the number currently required by clients (i.e. BeeKeeper).
 */
CREATE FUNCTION concat(VARCHAR, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, VARCHAR, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
/* Date/time concat function start */
CREATE FUNCTION concat(DATE, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, DATE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIME, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, TIME) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIMESTAMP, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, TIMESTAMP) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIME WITH TIME ZONE, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, TIME WITH TIME ZONE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIMESTAMP WITH TIME ZONE, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, TIMESTAMP WITH TIME ZONE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, DATE, DATE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(DATE, VARCHAR, DATE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(DATE, DATE, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, DATE, TIME) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(DATE, VARCHAR, TIME) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(DATE, TIME, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, DATE, TIMESTAMP) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(DATE, VARCHAR, TIMESTAMP) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(DATE, TIMESTAMP, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, DATE, TIME WITH TIME ZONE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(DATE, VARCHAR, TIME WITH TIME ZONE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(DATE, TIME WITH TIME ZONE, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, DATE, TIMESTAMP WITH TIME ZONE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(DATE, VARCHAR, TIMESTAMP WITH TIME ZONE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(DATE, TIMESTAMP WITH TIME ZONE, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, DATE, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(DATE, VARCHAR, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(DATE, VARCHAR, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, TIME, DATE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIME, VARCHAR, DATE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIME, DATE, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, TIME, TIME) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIME, VARCHAR, TIME) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIME, TIME, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, TIME, TIMESTAMP) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIME, VARCHAR, TIMESTAMP) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIME, TIMESTAMP, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, TIME, TIME WITH TIME ZONE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIME, VARCHAR, TIME WITH TIME ZONE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIME, TIME WITH TIME ZONE, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, TIME, TIMESTAMP WITH TIME ZONE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIME, VARCHAR, TIMESTAMP WITH TIME ZONE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIME, TIMESTAMP WITH TIME ZONE, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, TIME, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIME, VARCHAR, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIME, VARCHAR, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, TIMESTAMP, DATE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIMESTAMP, VARCHAR, DATE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIMESTAMP, DATE, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, TIMESTAMP, TIME) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIMESTAMP, VARCHAR, TIME) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIMESTAMP, TIME, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, TIMESTAMP, TIMESTAMP) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIMESTAMP, VARCHAR, TIMESTAMP) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIMESTAMP, TIMESTAMP, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, TIMESTAMP, TIME WITH TIME ZONE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIMESTAMP, VARCHAR, TIME WITH TIME ZONE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIMESTAMP, TIME WITH TIME ZONE, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, TIMESTAMP, TIMESTAMP WITH TIME ZONE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIMESTAMP, VARCHAR, TIMESTAMP WITH TIME ZONE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIMESTAMP, TIMESTAMP WITH TIME ZONE, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, TIMESTAMP, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIMESTAMP, VARCHAR, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIMESTAMP, VARCHAR, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, TIME WITH TIME ZONE, DATE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIME WITH TIME ZONE, VARCHAR, DATE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIME WITH TIME ZONE, DATE, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, TIME WITH TIME ZONE, TIME) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIME WITH TIME ZONE, VARCHAR, TIME) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIME WITH TIME ZONE, TIME, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, TIME WITH TIME ZONE, TIMESTAMP) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIME WITH TIME ZONE, VARCHAR, TIMESTAMP) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIME WITH TIME ZONE, TIMESTAMP, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, TIME WITH TIME ZONE, TIME WITH TIME ZONE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIME WITH TIME ZONE, VARCHAR, TIME WITH TIME ZONE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIME WITH TIME ZONE, TIME WITH TIME ZONE, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, TIME WITH TIME ZONE, TIMESTAMP WITH TIME ZONE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIME WITH TIME ZONE, VARCHAR, TIMESTAMP WITH TIME ZONE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIME WITH TIME ZONE, TIMESTAMP WITH TIME ZONE, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, TIME WITH TIME ZONE, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIME WITH TIME ZONE, VARCHAR, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIME WITH TIME ZONE, VARCHAR, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, TIMESTAMP WITH TIME ZONE, DATE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIMESTAMP WITH TIME ZONE, VARCHAR, DATE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIMESTAMP WITH TIME ZONE, DATE, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, TIMESTAMP WITH TIME ZONE, TIME) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIMESTAMP WITH TIME ZONE, VARCHAR, TIME) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIMESTAMP WITH TIME ZONE, TIME, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, TIMESTAMP WITH TIME ZONE, TIMESTAMP) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIMESTAMP WITH TIME ZONE, VARCHAR, TIMESTAMP) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIMESTAMP WITH TIME ZONE, TIMESTAMP, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, TIMESTAMP WITH TIME ZONE, TIME WITH TIME ZONE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIMESTAMP WITH TIME ZONE, VARCHAR, TIME WITH TIME ZONE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIMESTAMP WITH TIME ZONE, TIME WITH TIME ZONE, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIMESTAMP WITH TIME ZONE, VARCHAR, TIMESTAMP WITH TIME ZONE) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(VARCHAR, TIMESTAMP WITH TIME ZONE, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIMESTAMP WITH TIME ZONE, VARCHAR, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION concat(TIMESTAMP WITH TIME ZONE, VARCHAR, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
/* Date/Time concat function end */
/* This only implements the 2-argument version of ROUND, since Octo doesn't support function overloading. */
CREATE FUNCTION round(NUMERIC, INTEGER) RETURNS NUMERIC AS $$ROUND^%ydboctosqlfunctions;
CREATE FUNCTION round(INTEGER, INTEGER) RETURNS NUMERIC AS $$ROUND^%ydboctosqlfunctions;
/* This only implements the 2-argument version of TRUNC, since Octo doesn't support function overloading. */
CREATE FUNCTION trunc(NUMERIC, INTEGER) RETURNS NUMERIC AS $$^%ydboctofTRUNCATE;
CREATE FUNCTION trunc(INTEGER, NUMERIC) RETURNS NUMERIC AS $$^%ydboctofTRUNCATE;
CREATE FUNCTION trunc(NUMERIC, NUMERIC) RETURNS NUMERIC AS $$^%ydboctofTRUNCATE;
CREATE FUNCTION trunc(INTEGER, INTEGER) RETURNS NUMERIC AS $$^%ydboctofTRUNCATE;
/* The following are non-standard MySQL and MariaDB functions. */
CREATE FUNCTION truncate(NUMERIC, INTEGER) RETURNS NUMERIC AS $$^%ydboctofTRUNCATE;
CREATE FUNCTION truncate(INTEGER, NUMERIC) RETURNS NUMERIC AS $$^%ydboctofTRUNCATE;
CREATE FUNCTION truncate(NUMERIC, NUMERIC) RETURNS NUMERIC AS $$^%ydboctofTRUNCATE;
CREATE FUNCTION truncate(INTEGER, INTEGER) RETURNS NUMERIC AS $$^%ydboctofTRUNCATE;

CREATE FUNCTION now() RETURNS TIMESTAMP WITH TIME ZONE AS $$^%ydboctofCURRENTTIMESTAMP;
CREATE FUNCTION day(DATE) RETURNS VARCHAR AS $$DAY^%ydboctosqlfunctions;
CREATE FUNCTION day(VARCHAR) RETURNS VARCHAR AS $$DAY^%ydboctosqlfunctions; /* Original varchar form is retained as this
									     * avoids having to specify date qualifier
									     * for the literal.
									     */
/* Aliases for NOW
   TODO: move these to a MariaDB-specific seed file
*/
CREATE FUNCTION localtime() RETURNS TIME AS $$^%ydboctofLOCALTIME;
CREATE FUNCTION localtimestamp() RETURNS TIMESTAMP AS $$^%ydboctofCURRENTTIMESTAMP;
CREATE FUNCTION current_timestamp() RETURNS TIMESTAMP WITH TIME ZONE AS $$^%ydboctofCURRENTTIMESTAMP;
CREATE FUNCTION current_time() RETURNS TIME WITH TIME ZONE AS $$^%ydboctofCURRENTTIME;
CREATE FUNCTION date_format(TIMESTAMP, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofDATEFORMAT;;
CREATE FUNCTION date_format(TIMESTAMP WITH TIME ZONE, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofDATEFORMAT;;
CREATE FUNCTION date_format(VARCHAR, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofDATEFORMAT; /* Original varchar form is retained as this
											  * avoids having to specify date qualifier
											  * for the literal.
											  */
/* date/time formating function*/
/* Following convert from text to a different format */
CREATE FUNCTION date_to_fileman(DATE(FILEMAN)) RETURNS NUMERIC AS $$FILEMAN^%ydboctofTODTFORMAT;
CREATE FUNCTION timestamp_to_fileman(TIMESTAMP(FILEMAN)) RETURNS NUMERIC AS $$FILEMAN^%ydboctofTODTFORMAT;
CREATE FUNCTION timestamptz_to_fileman(TIMESTAMP(FILEMAN) WITH TIME ZONE) RETURNS NUMERIC AS $$FILEMAN^%ydboctofTODTFORMAT;

CREATE FUNCTION date_to_horolog(DATE(HOROLOG)) RETURNS VARCHAR AS $$HOROLOG^%ydboctofTODTFORMAT;
CREATE FUNCTION time_to_horolog(TIME(HOROLOG)) RETURNS VARCHAR AS $$HOROLOG^%ydboctofTODTFORMAT;
CREATE FUNCTION timetz_to_horolog(TIME(HOROLOG) WITH TIME ZONE) RETURNS VARCHAR AS $$HOROLOG^%ydboctofTODTFORMAT;
CREATE FUNCTION timestamp_to_horolog(TIMESTAMP(HOROLOG)) RETURNS VARCHAR AS $$HOROLOG^%ydboctofTODTFORMAT;
CREATE FUNCTION timestamptz_to_horolog(TIMESTAMP(HOROLOG) WITH TIME ZONE) RETURNS VARCHAR AS $$HOROLOG^%ydboctofTODTFORMAT;

CREATE FUNCTION date_to_zhorolog(DATE(ZHOROLOG)) RETURNS VARCHAR AS $$ZHOROLOG^%ydboctofTODTFORMAT;
CREATE FUNCTION time_to_zhorolog(TIME(ZHOROLOG)) RETURNS VARCHAR AS $$ZHOROLOG^%ydboctofTODTFORMAT;
CREATE FUNCTION timetz_to_zhorolog(TIME(ZHOROLOG) WITH TIME ZONE) RETURNS VARCHAR AS $$ZHOROLOG^%ydboctofTODTFORMAT;
CREATE FUNCTION timestamp_to_zhorolog(TIMESTAMP(ZHOROLOG)) RETURNS VARCHAR AS $$ZHOROLOG^%ydboctofTODTFORMAT;
CREATE FUNCTION timestamptz_to_zhorolog(TIMESTAMP(ZHOROLOG) WITH TIME ZONE) RETURNS VARCHAR AS $$ZHOROLOG^%ydboctofTODTFORMAT;

CREATE FUNCTION date_to_zut(DATE(ZUT)) RETURNS INTEGER AS $$ZUT^%ydboctofTODTFORMAT;
CREATE FUNCTION timestamp_to_zut(TIMESTAMP(ZUT)) RETURNS INTEGER AS $$ZUT^%ydboctofTODTFORMAT;
/* Alias for DAY */
CREATE FUNCTION dayofmonth(DATE) RETURNS VARCHAR AS $$DAY^%ydboctosqlfunctions;
CREATE FUNCTION dayofmonth(VARCHAR) RETURNS VARCHAR AS $$DAY^%ydboctosqlfunctions; /* Original varchar form is retained as this
									            * avoids having to specify date qualifier
									            * for the literal.
									            */
CREATE FUNCTION lpad(VARCHAR, INTEGER) RETURNS VARCHAR AS $$^%ydboctofLPAD;
CREATE FUNCTION lpad(VARCHAR, INTEGER, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofLPAD;

/* REPLACE is used by SquirrelSQL during connection intialize and so is included here.
 * Note that REPLACE is not currently implemented and the matching M routine is an empty placeholder that
 * simply returns the first argument passed to it.
 */
CREATE FUNCTION replace(VARCHAR, VARCHAR, VARCHAR) RETURNS VARCHAR AS $$REPLACE^%ydboctosqlfunctions;
CREATE FUNCTION row_number() RETURNS INTEGER AS $$pgRowNumber^%ydboctopgfunctions;
/* Set a runtime variable to the specified value*/
CREATE FUNCTION set_config(VARCHAR, VARCHAR, BOOLEAN) RETURNS VARCHAR AS $$setConfig^%ydboctopgfunctions;
CREATE FUNCTION version() RETURNS VARCHAR AS $$VERSION^%ydboctosqlfunctions;

/* Note: Lines that start with "CREATE FUNCTION pg_catalog..." need to have an empty line after the query as this is relied upon
 * by the sed commands in "tests/fixtures/convert_catalog_sql.sh". Hence the blank lines below after each CREATE FUNCTION.
 */
CREATE FUNCTION pg_catalog.pg_table_is_visible(INTEGER) RETURNS BOOLEAN AS $$pgTableIsVisible^%ydboctopgfunctions;

/* Note: Need an empty line after each pg_catalog. function. See comment before pg_catalog.pg_table_is_visible for more detail. */
CREATE FUNCTION pg_catalog.pg_get_userbyid(INTEGER) RETURNS VARCHAR AS $$pgGetUserById^%ydboctopgfunctions;

CREATE FUNCTION current_schema() RETURNS VARCHAR AS $$pgCurrentSchema^%ydboctopgfunctions;
CREATE FUNCTION current_database() RETURNS VARCHAR AS $$pgCurrentDatabase^%ydboctopgfunctions;
CREATE FUNCTION current_catalog() RETURNS VARCHAR AS $$pgCurrentCatalog^%ydboctopgfunctions;
/* Octo does not currently distinguish between various types of users,
 * nor does it distinguish between roles. Accordingly, the same extrinsic
 * function is used for the below cases.
 */
CREATE FUNCTION current_role() RETURNS VARCHAR AS $$pgUser^%ydboctopgfunctions;
CREATE FUNCTION current_user() RETURNS VARCHAR AS $$pgUser^%ydboctopgfunctions;
CREATE FUNCTION session_user() RETURNS VARCHAR AS $$pgUser^%ydboctopgfunctions;
CREATE FUNCTION user() RETURNS VARCHAR AS $$pgUser^%ydboctopgfunctions;

/* Note: Need an empty line after each pg_catalog. function. See comment before pg_catalog.pg_table_is_visible for more detail. */
CREATE FUNCTION pg_catalog.current_schemas(BOOLEAN) RETURNS VARCHAR AS $$pgCurrentSchemas^%ydboctopgfunctions;

/* Note: Need an empty line after each pg_catalog. function. See comment before pg_catalog.pg_table_is_visible for more detail. */
CREATE FUNCTION pg_catalog.obj_description(INTEGER, VARCHAR) RETURNS VARCHAR AS $$pgObjDescription^%ydboctopgfunctions;

/* Note: Need an empty line after each pg_catalog. function. See comment before pg_catalog.pg_table_is_visible for more detail. */
CREATE FUNCTION pg_catalog.pg_backend_pid() RETURNS VARCHAR AS $$pgBackendPid^%ydboctopgfunctions;

/* Note: Need an empty line after each pg_catalog. function. See comment before pg_catalog.pg_table_is_visible for more detail. */
CREATE FUNCTION pg_catalog.pg_get_expr(VARCHAR, INTEGER) RETURNS VARCHAR AS $$pgGetExpr^%ydboctopgfunctions;

/* Note: Need an empty line after each pg_catalog. function. See comment before pg_catalog.pg_table_is_visible for more detail. */
CREATE FUNCTION pg_catalog.pg_get_expr(VARCHAR, INTEGER, BOOLEAN) RETURNS VARCHAR AS $$pgGetExpr^%ydboctopgfunctions;

/* Note: Need an empty line after each pg_catalog. function. See comment before pg_catalog.pg_table_is_visible for more detail. */
CREATE FUNCTION pg_catalog.pg_total_relation_size(INTEGER) RETURNS VARCHAR AS $$pgTotalRelationSize^%ydboctopgfunctions;

/* Note: Need an empty line after each pg_catalog. function. See comment before pg_catalog.pg_table_is_visible for more detail. */
CREATE FUNCTION pg_catalog.pg_relation_size(INTEGER) RETURNS VARCHAR AS $$pgRelationSize^%ydboctopgfunctions;

/* Note: Need an empty line after each pg_catalog. function. See comment before pg_catalog.pg_table_is_visible for more detail. */
CREATE FUNCTION pg_catalog.pg_get_constraintdef(INTEGER) RETURNS VARCHAR AS $$pgGetConstraintDef^%ydboctopgfunctions;

/* Note: Need an empty line after each pg_catalog. function. See comment before pg_catalog.pg_table_is_visible for more detail. */
CREATE FUNCTION pg_catalog.pg_get_constraintdef(INTEGER, BOOLEAN) RETURNS VARCHAR AS $$pgGetConstraintDef^%ydboctopgfunctions;

/* Note: Need an empty line after each pg_catalog. function. See comment before pg_catalog.pg_table_is_visible for more detail. */
CREATE FUNCTION pg_catalog.pg_get_ruledef(INTEGER) RETURNS VARCHAR AS $$pgGetRuleDef^%ydboctopgfunctions;

/* Note: Need an empty line after each pg_catalog. function. See comment before pg_catalog.pg_table_is_visible for more detail. */
CREATE FUNCTION pg_catalog.pg_get_ruledef(INTEGER, BOOLEAN) RETURNS VARCHAR AS $$pgGetRuleDef^%ydboctopgfunctions;

/* Until pg_database is fully implemented (#417) this will always return "SQL_ASCII" */
CREATE FUNCTION pg_encoding_to_char(INTEGER) RETURNS VARCHAR AS $$pgEncodingToChar^%ydboctopgfunctions;
/* Return true if database is currently recovering from a backup.
 * Since this feature is not implemented in Octo, this function will always return false.
 */
CREATE FUNCTION pg_is_in_recovery() RETURNS BOOLEAN AS $$pgIsInRecovery^%ydboctopgfunctions;
/* Return true if database recovery is currently paused.
 * Since this feature is not implemented in Octo, this function will always return false.
 */
CREATE FUNCTION pg_is_xlog_replay_paused() RETURNS BOOLEAN AS $$pgIsXlogReplayPaused^%ydboctopgfunctions;
/* Until pg_database is fully implemented (#417) this will always return "SQL_ASCII" */
/* Since Octo currently does not implement privileges, the following always return TRUE */
CREATE FUNCTION has_database_privilege(INTEGER, VARCHAR) RETURNS BOOLEAN AS $$pgHasDatabasePrivilege^%ydboctopgfunctions;
CREATE FUNCTION has_database_privilege(VARCHAR, VARCHAR, VARCHAR) RETURNS BOOLEAN AS $$pgHasDatabasePrivilege^%ydboctopgfunctions;

CREATE FUNCTION array_lower(INTEGER, INT) RETURNS INT AS $$pgArrayLower^%ydboctopgfunctions;
CREATE FUNCTION array_lower(NUMERIC, INT) RETURNS INT AS $$pgArrayLower^%ydboctopgfunctions;
CREATE FUNCTION array_lower(VARCHAR, INT) RETURNS INT AS $$pgArrayLower^%ydboctopgfunctions;
CREATE FUNCTION array_lower(BOOLEAN, INT) RETURNS INT AS $$pgArrayLower^%ydboctopgfunctions;
CREATE FUNCTION array_upper(INTEGER, INT) RETURNS INT AS $$pgArrayLower^%ydboctopgfunctions;
CREATE FUNCTION array_upper(NUMERIC, INT) RETURNS INT AS $$pgArrayLower^%ydboctopgfunctions;
CREATE FUNCTION array_upper(VARCHAR, INT) RETURNS INT AS $$pgArrayLower^%ydboctopgfunctions;
CREATE FUNCTION array_upper(BOOLEAN, INT) RETURNS INT AS $$pgArrayLower^%ydboctopgfunctions;
CREATE FUNCTION generate_series(INT, INT) RETURNS INT AS $$pgGenerateSeries^%ydboctopgfunctions;

CREATE FUNCTION regclass(VARCHAR) RETURNS INTEGER AS $$pgRegClass^%ydboctopgfunctions;
CREATE FUNCTION regproc(VARCHAR) RETURNS VARCHAR AS $$pgRegProc^%ydboctopgfunctions;
CREATE FUNCTION format_type(INTEGER, INTEGER) RETURNS VARCHAR AS $$pgFormatType^%ydboctopgfunctions;
CREATE FUNCTION substring(VARCHAR) RETURNS VARCHAR AS $$pgSubString^%ydboctopgfunctions;
CREATE FUNCTION substring(VARCHAR, INTEGER) RETURNS VARCHAR AS $$pgSubString^%ydboctopgfunctions;
CREATE FUNCTION substring(VARCHAR, INTEGER, INTEGER) RETURNS VARCHAR AS $$pgSubString^%ydboctopgfunctions;

/* Note: Need an empty line after each pg_catalog. function. See comment before pg_catalog.pg_table_is_visible for more detail. */
CREATE FUNCTION pg_catalog.pg_get_partkeydef(INTEGER) RETURNS VARCHAR AS $$pgGetPartKeyDef^%ydboctopgfunctions;

/* Note: Need an empty line after each pg_catalog. function. See comment before pg_catalog.pg_table_is_visible for more detail. */
CREATE FUNCTION pg_catalog.pg_stat_get_numscans(INTEGER) RETURNS VARCHAR AS $$pgStatGetNumScans^%ydboctopgfunctions;

/* Note: Need an empty line after each pg_catalog. function. See comment before pg_catalog.pg_table_is_visible for more detail. */
CREATE FUNCTION pg_catalog.pg_tablespace_location(INTEGER) RETURNS VARCHAR AS $$pgTableSpaceLocation^%ydboctopgfunctions;

/* Stores PostgreSQL range types */
CREATE TABLE pg_catalog.pg_range (
	rngsubtype INTEGER,
	rngcollation INTEGER,
	rngsubopc INTEGER,
	rngcanonical INTEGER,
	rngsubdiff INTEGER,
	rngtypid INTEGER primary key
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_range"",keys(""rngtypid""))" READONLY;

/* Stores PostgreSQL enum types */
CREATE TABLE pg_catalog.pg_enum (
	enumtypid INTEGER,
	enumsortorder NUMERIC,
	enumlabel VARCHAR,
	oid INTEGER primary key
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_enum"",keys(""oid""))" READONLY;

/* Stores PostgreSQL index information */
CREATE TABLE pg_catalog.pg_index (
	indexrelid INTEGER,		-- Related to `pg_class.oid`: The OID of the `pg_class` entry for this index
	indrelid INTEGER,		-- Related to `pg_class.oid`: The OID of the `pg_class` entry for the table this index is for
	indnatts INTEGER,
	indisunique	BOOLEAN,
	indisprimary BOOLEAN,
	indisexclusion BOOLEAN,
	indimmediate BOOLEAN,
	indisclustered BOOLEAN,
	indisvalid BOOLEAN,
	indcheckxmin BOOLEAN,
	indisready BOOLEAN,
	indislive BOOLEAN,
	indisreplident BOOLEAN,
	indkey INTEGER,			-- This is an array field (int2vector type)
	indcollation INTEGER,		-- This is an array field (oidvector type)
	indclass INTEGER,		-- This is an array field (oidvector type)
	indoption INTEGER,		-- This is an array field (int2vector type)
	indexprs VARCHAR,	 	-- This is an `expression tree` field (pg_node_tree type)
	indpred	VARCHAR,	 	-- This is an `expression tree` field (pg_node_tree type)
	oid INTEGER primary key
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_index"",keys(""oid""))" READONLY;

/* Stores PostgreSQL constraint information.
 * See https://www.postgresql.org/docs/9.6/catalog-pg-constraint.html for more information.
 */
CREATE TABLE pg_catalog.pg_constraint (
	conname	NAME,
	connamespace INTEGER,	-- Relates to pg_namespace.oid
	contype	CHAR,
	condeferrable BOOLEAN,
	condeferred	BOOLEAN,
	convalidated BOOLEAN,
	conrelid INTEGER,		-- Relates to pg_class.oid	The table this constraint is on; 0 if not a table constraint
	contypid INTEGER,		-- Relates to pg_type.oid	The domain this constraint is on; 0 if not a domain constraint
	conindid INTEGER,		-- Relates to pg_class.oid	The index supporting this constraint, if it's a unique, primary key, foreign key, or exclusion constraint; else 0
	confrelid INTEGER,		-- Relates to pg_class.oid	If a foreign key, the referenced table; else 0
	confupdtype	CHAR,
	confdeltype	CHAR,
	confmatchtype CHAR,
	conislocal BOOLEAN,
	coninhcount	INTEGER,
	connoinherit BOOLEAN,
	conkey INTEGER,			-- Relates to pg_attribute.attnum. This is an array field (smallint[] type)
	confkey INTEGER,		-- Relates to pg_attribute.attnum. This is an array field (smallint[] type)
	conpfeqop INTEGER,		-- Relates to pg_operator.oid. This is an array field (oid[] type)
	conppeqop INTEGER,		-- Relates to pg_operator.oid. This is an array field (oid[] type)
	conffeqop INTEGER,		-- Relates to pg_operator.oid. This is an array field (oid[] type)
	conexclop INTEGER,		-- Relates to pg_operator.oid. This is an array field (oid[] type)
	conbin	VARCHAR,		-- This is an `expression tree` field (pg_node_tree type)
	consrc	VARCHAR,
	oid INTEGER primary key
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_constraint"",keys(""oid"")" READONLY;

/* Stores information about PostgreSQL access methods.
 * See https://www.postgresql.org/docs/current/catalog-pg-am.html for more information.
 */
CREATE TABLE pg_catalog.pg_am (
	amname	NAME,
	amhandler	VARCHAR,	-- An OID alias of type `regproc` in PostgreSQL
	amtype	VARCHAR,
	oid INTEGER primary key
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_am"",keys(""oid"")" READONLY;

/* Records the dependency relationships between database objects. This information allows DROP commands to find
 * which other objects must be dropped by DROP CASCADE or prevent dropping in the DROP RESTRICT case.
 * See https://www.postgresql.org/docs/7.4/catalog-pg-depend.html for more details.
 *
 * Octo does not currently record dependencies in this table but will need to when DROP CASCADE/RESTRICT are implemented.
 * Until then, maintain this as an empty table.
 */
CREATE TABLE pg_catalog.pg_depend(
	classid INTEGER NOT NULL,
	objid INTEGER NOT NULL,
	objsubid INTEGER NOT NULL,
	refclassid INTEGER NOT NULL,
	refobjid INTEGER NOT NULL,
	refobjsubid INTEGER NOT NULL,
	deptype VARCHAR NOT NULL,
	PRIMARY KEY(classid, objid, objsubid)
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_depend"",keys(""classid""),keys(""objid""),keys(""objsubid""))" READONLY;


/* See https://www.postgresql.org/docs/current/catalog-pg-trigger.html for more details.
 * Octo currently maintains this as an empty table.
 */
CREATE TABLE pg_catalog.pg_trigger(
	oid INTEGER,
	tgrelid INTEGER,
	tgname VARCHAR,
	tgfoid INTEGER,
	tgtype SMALLINT,
	tgenabled CHAR,
	tgisinternal BOOLEAN,
	tgconstrrelid INTEGER,
	tgconstrindid INTEGER,
	tgconstraint INTEGER,
	tgdeferrable BOOLEAN,
	tginitdeferred BOOLEAN,
	tgnargs SMALLINT,
	tgattr VARCHAR,		-- int2vector in Postgres but approximated with VARCHAR in Octo
	tgargs VARCHAR,		-- bytea in Postgres but approximated with VARCHAR in Octo
	tgqual varchar,
	tgoldtable VARCHAR,
	tgnewtable VARCHAR,
	PRIMARY KEY(oid)
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_trigger"",keys(""oid""))" READONLY;

/* See https://www.postgresql.org/docs/current/catalog-pg-tablespace.html for more details.
 * Octo currently maintains this as an empty table.
 */
CREATE TABLE pg_catalog.pg_tablespace(
	oid INTEGER,
	spcname VARCHAR,
	spcowner INTEGER,
	spcacl VARCHAR,		-- aclitem[] in Postgres but approximated with VARCHAR in Octo
	spcoptions VARCHAR,	-- text[] in Postgres but approximated with VARCHAR in Octo
	PRIMARY KEY(oid)
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_tablespace"",keys(""oid""))" READONLY;

/* See https://www.postgresql.org/docs/current/catalog-pg-shdescription.html for more details.
 * Octo currently maintains this as an empty table.
 */
CREATE TABLE pg_catalog.pg_shdescription(
	objoid INTEGER,
	classoid INTEGER,
	description VARCHAR,
	spcoptions VARCHAR,	-- text[] in Postgres but approximated with VARCHAR in Octo
	PRIMARY KEY(objoid, classoid)
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_shdescription"",keys(""objoid""),keys(""classoid""))" READONLY;

/* See https://www.postgresql.org/docs/8.0/catalog-pg-inherits.html for more details.
 * Octo currently maintains this as an empty table.
 */
CREATE TABLE pg_catalog.pg_inherits(
	inhrelid INTEGER,
	inhparent INTEGER,
	inhseqno INT4,
	PRIMARY KEY(inhrelid, inhseqno)
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_inherits"",keys(""inhrelid""),keys(""inhseqno""))" READONLY;

/* See https://www.postgresql.org/docs/current/view-pg-policies.html for more details.
 * Octo currently maintains this as an empty table.
 */
CREATE TABLE pg_catalog.pg_policies(
	schemaname VARCHAR,
	tablename VARCHAR,
	policyname VARCHAR,
	permissive VARCHAR,
	roles VARCHAR,		-- name[] in Postgres but approximated with VARCHAR in Octo
	cmd VARCHAR,
	qual VARCHAR,
	with_check VARCHAR,
	PRIMARY KEY(schemaname, tablename)
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_policies"",keys(""schemaname""),keys(""tablename""))" READONLY;

/* See https://www.postgresql.org/docs/current/catalog-pg-rewrite.html for more details.
 * Octo currently maintains this as an empty table.
 */
CREATE TABLE pg_catalog.pg_rewrite(
	oid INTEGER NOT NULL,
	rulename VARCHAR NOT NULL,
	ev_class INTEGER NOT NULL,
	ev_type CHAR NOT NULL,
	ev_enabled CHAR NOT NULL,
	is_instead BOOLEAN NOT NULL,
	ev_qual VARCHAR NOT NULL,	-- pg_node_tree in Postgres but approximated with VARCHAR in Octo
	ev_action VARCHAR NOT NULL,	-- pg_node_tree in Postgres but approximated with VARCHAR in Octo
	PRIMARY KEY(oid)
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_rewrite"",keys(""oid""))" READONLY;

/* See https://www.postgresql.org/docs/current/catalog-pg-conversion.html for more details.
 * Octo currently maintains this as an empty table.
 */
CREATE TABLE pg_catalog.pg_conversion(
	oid INTEGER NOT NULL,
	conname VARCHAR NOT NULL,
	connamespace INTEGER NOT NULL,
	conowner INTEGER NOT NULL,
	conforencoding INTEGER NOT NULL,
	contoencoding INTEGER NOT NULL,
	conproc VARCHAR NOT NULL,	-- regproc in PostgreSQL but approximated with VARCHAR in Octo
	condefault BOOLEAN NOT NULL,
	PRIMARY KEY(oid)
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_conversion"",keys(""oid""))" READONLY;

/* See https://www.postgresql.org/docs/current/catalog-pg-aggregate.html for more details.
 * Octo currently maintains this as an empty table.
 */
CREATE TABLE pg_catalog.pg_aggregate(
	aggfnoid INTEGER NOT NULL,		-- regproc in Postgres but approximated with INTEGER in Octo,
	aggkind CHAR NOT NULL,
	aggnumdirectargs SMALLINT NOT NULL,
	aggtransfn VARCHAR NOT NULL,		-- regproc in Postgres but approximated with VARCHAR in Octo,
	aggfinalfn VARCHAR NOT NULL,		-- regproc in Postgres but approximated with VARCHAR in Octo,
	aggcombinefn VARCHAR NOT NULL,		-- regproc in Postgres but approximated with VARCHAR in Octo,
	aggserialfn VARCHAR NOT NULL,		-- regproc in Postgres but approximated with VARCHAR in Octo,
	aggdeserialfn VARCHAR NOT NULL,		-- regproc in Postgres but approximated with VARCHAR in Octo,
	aggmtransfn VARCHAR NOT NULL,		-- regproc in Postgres but approximated with VARCHAR in Octo,
	aggminvtransfn VARCHAR NOT NULL,	-- regproc in Postgres but approximated with VARCHAR in Octo,
	aggmfinalfn VARCHAR NOT NULL,		-- regproc in Postgres but approximated with VARCHAR in Octo,
	aggfinalextra boolean NOT NULL,
	aggmfinalextra boolean NOT NULL,
	aggfinalmodify CHAR NOT NULL,
	aggmfinalmodify CHAR NOT NULL,
	aggsortop INTEGER NOT NULL,
	aggtranstype INTEGER NOT NULL,
	aggtransspace INTEGER NOT NULL,
	aggmtranstype INTEGER NOT NULL,
	aggmtransspace INTEGER NOT NULL,
	agginitval VARCHAR,
	aggminitval VARCHAR,
	PRIMARY KEY(aggfnoid)
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_aggregate"",keys(""aggfnoid""))" READONLY;

/* See https://www.postgresql.org/docs/current/catalog-pg-language.html for more details.
 * Octo currently maintains this as an empty table.
 */
CREATE TABLE pg_catalog.pg_language(
	oid INTEGER NOT NULL,
	lanname VARCHAR NOT NULL,
	lanowner INTEGER NOT NULL,
	lanispl BOOLEAN NOT NULL,
	lanpltrusted BOOLEAN NOT NULL,
	lanplcallfoid INTEGER NOT NULL,
	laninline INTEGER NOT NULL,
	lanvalidator INTEGER NOT NULL,
	lanacl VARCHAR,			-- aclitem[] in Postgres but approximated with VARCHAR in Octo
	PRIMARY KEY(oid)
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_language"",keys(""oid""))" READONLY;

