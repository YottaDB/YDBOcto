#################################################################
#								#
# Copyright (c) 2019-2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

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
  typreceive VARCHAR(25),
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
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_class"",keys(""oid"")" READONLY;

/* Populated via special DDL arguments */
CREATE TABLE pg_catalog.pg_description (
  objoid INTEGER,
  classoid INTEGER,
  objsubid INTEGER,
  description VARCHAR,
  oid INTEGER PRIMARY KEY
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_description"",keys(""oid"")" READONLY;


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
) GLOBAL "^%ydboctoocto(""tables"",""information_schema"",""tables"",keys(""oid"")" READONLY;


CREATE TABLE pg_catalog.pg_proc (
  proname VARCHAR,
  pronamespace INTEGER,
  proowner INTEGER,
  prolang INTEGER,
  procost INTEGER,
  prorows INTEGER,
  provariadic INTEGER,
  protransform VARCHAR,
  prokind BOOLEAN,
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
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_proc"",keys(""oid"")" READONLY;

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
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_attribute"",keys(""oid"")" READONLY;

/* Stores default values for columns */
CREATE TABLE pg_catalog.pg_attrdef (
 adrelid INTEGER,
 adnum INTEGER,
 adbin VARCHAR,
 adsrc VARCHAR,
 oid INTEGER primary key
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_attrdef"",keys(""oid"")" READONLY;

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
) GLOBAL "%ydboctoocto(""settings"",""pg_settings"",keys(""name"")";

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
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_database"",keys(""oid"")" READONLY;

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
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_roles"",keys(""oid"")" READONLY;

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
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_user"",keys(""usersysid"")" READONLY;

/* Add standard SQL functions and internal Octo functions to the database and catalog
 * Note that some catalog functions do NOT use the types specified for them in PostgreSQL,
 * as Octo doesn't implement them (yet).
 * Note also that some functions are overloaded to account for various valid combinations of
 * parameter types, e.g. accept both `NUMERIC` and `INTEGER` types for the `ABS` function.
 */
CREATE FUNCTION ABS(NUMERIC) RETURNS NUMERIC AS $$ABS^%ydboctosqlfunctions;
CREATE FUNCTION ABS(INTEGER) RETURNS INTEGER AS $$ABS^%ydboctosqlfunctions;
/* Note that PostgreSQL CONCAT accepts a variable number of arguments. Since Octo doesn't support this,
 * just use the number currently required by clients (i.e. BeeKeeper).
 */
CREATE FUNCTION CONCAT(VARCHAR, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION CONCAT(VARCHAR, VARCHAR, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
/* This only implements the 2-argument version of ROUND, since Octo doesn't support function overloading. */
CREATE FUNCTION ROUND(NUMERIC, INTEGER) RETURNS NUMERIC AS $$ROUND^%ydboctosqlfunctions;
CREATE FUNCTION ROUND(INTEGER, INTEGER) RETURNS NUMERIC AS $$ROUND^%ydboctosqlfunctions;
/* This only implements the 2-argument version of TRUNC, since Octo doesn't support function overloading. */
CREATE FUNCTION TRUNC(NUMERIC, INTEGER) RETURNS NUMERIC AS $$^%ydboctofTRUNCATE;
CREATE FUNCTION TRUNC(INTEGER, NUMERIC) RETURNS NUMERIC AS $$^%ydboctofTRUNCATE;
CREATE FUNCTION TRUNC(NUMERIC, NUMERIC) RETURNS NUMERIC AS $$^%ydboctofTRUNCATE;
CREATE FUNCTION TRUNC(INTEGER, INTEGER) RETURNS NUMERIC AS $$^%ydboctofTRUNCATE;
/* The following are non-standard MySQL and MariaDB functions. */
CREATE FUNCTION TRUNCATE(NUMERIC, INTEGER) RETURNS NUMERIC AS $$^%ydboctofTRUNCATE;
CREATE FUNCTION TRUNCATE(INTEGER, NUMERIC) RETURNS NUMERIC AS $$^%ydboctofTRUNCATE;
CREATE FUNCTION TRUNCATE(NUMERIC, NUMERIC) RETURNS NUMERIC AS $$^%ydboctofTRUNCATE;
CREATE FUNCTION TRUNCATE(INTEGER, INTEGER) RETURNS NUMERIC AS $$^%ydboctofTRUNCATE;

/* TODO(#382): When DATETIME is implemented, change these to return DATETIME */
CREATE FUNCTION NOW() RETURNS VARCHAR AS $$^%ydboctofCURRENTTIMESTAMP;
CREATE FUNCTION DAY(VARCHAR) RETURNS VARCHAR AS $$DAY^%ydboctosqlfunctions;
/* Aliases for NOW
   TODO: move these to a MariaDB-specific seed file
*/
CREATE FUNCTION LOCALTIME() RETURNS VARCHAR AS $$^%ydboctofLOCALTIME;
CREATE FUNCTION LOCALTIMESTAMP() RETURNS VARCHAR AS $$^%ydboctofCURRENTTIMESTAMP;
CREATE FUNCTION CURRENT_TIMESTAMP() RETURNS VARCHAR AS $$^%ydboctofCURRENTTIMESTAMP;
CREATE FUNCTION CURRENT_TIME() RETURNS VARCHAR AS $$^%ydboctofCURRENTTIME;
CREATE FUNCTION DATE_FORMAT(VARCHAR, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofDATEFORMAT;;
/* Alias for DAY */
CREATE FUNCTION DAYOFMONTH(VARCHAR) RETURNS VARCHAR AS $$DAY^%ydboctosqlfunctions;

CREATE FUNCTION LPAD(VARCHAR, INTEGER) RETURNS VARCHAR AS $$^%ydboctofLPAD;
CREATE FUNCTION LPAD(VARCHAR, INTEGER, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofLPAD;

/* REPLACE is used by SquirrelSQL during connection intialize and so is included here.
 * Note that REPLACE is not currently implemented and the matching M routine is an empty placeholder that
 * simply returns the first argument passed to it.
 */
CREATE FUNCTION REPLACE(VARCHAR, VARCHAR, VARCHAR) RETURNS VARCHAR AS $$REPLACE^%ydboctosqlfunctions;
CREATE FUNCTION ROW_NUMBER() RETURNS INTEGER AS $$pgRowNumber^%ydboctopgfunctions;
/* Set a runtime variable to the specified value*/
CREATE FUNCTION SET_CONFIG(VARCHAR, VARCHAR, BOOLEAN) RETURNS VARCHAR AS $$setConfig^%ydboctopgfunctions;
CREATE FUNCTION VERSION() RETURNS VARCHAR AS $$VERSION^%ydboctosqlfunctions;

/* Note: Lines that start with "CREATE FUNCTION PG_CATALOG..." need to have an empty line after the query as this is relied upon
 * by the sed commands in "tests/fixtures/convert_catalog_sql.sh". Hence the blank lines below after each CREATE FUNCTION.
 */
CREATE FUNCTION PG_CATALOG.PG_TABLE_IS_VISIBLE(INTEGER) RETURNS BOOLEAN AS $$pgTableIsVisible^%ydboctopgfunctions;

/* Note: Need an empty line after each PG_CATALOG. function. See comment before PG_CATALOG.PG_TABLE_IS_VISIBLE for more detail. */
CREATE FUNCTION PG_CATALOG.PG_GET_USERBYID(INTEGER) RETURNS VARCHAR AS $$pgGetUserById^%ydboctopgfunctions;

CREATE FUNCTION CURRENT_SCHEMA() RETURNS VARCHAR AS $$pgCurrentSchema^%ydboctopgfunctions;
CREATE FUNCTION CURRENT_DATABASE() RETURNS VARCHAR AS $$pgCurrentDatabase^%ydboctopgfunctions;
CREATE FUNCTION CURRENT_CATALOG() RETURNS VARCHAR AS $$pgCurrentCatalog^%ydboctopgfunctions;
/* Octo does not currently distinguish between various types of users,
 * nor does it distinguish between roles. Accordingly, the same extrinsic
 * function is used for the below cases.
 */
CREATE FUNCTION CURRENT_ROLE() RETURNS VARCHAR AS $$pgUser^%ydboctopgfunctions;
CREATE FUNCTION CURRENT_USER() RETURNS VARCHAR AS $$pgUser^%ydboctopgfunctions;
CREATE FUNCTION SESSION_USER() RETURNS VARCHAR AS $$pgUser^%ydboctopgfunctions;
CREATE FUNCTION USER() RETURNS VARCHAR AS $$pgUser^%ydboctopgfunctions;

/* Note: Need an empty line after each PG_CATALOG. function. See comment before PG_CATALOG.PG_TABLE_IS_VISIBLE for more detail. */
CREATE FUNCTION PG_CATALOG.CURRENT_SCHEMAS(BOOLEAN) RETURNS VARCHAR AS $$pgCurrentSchemas^%ydboctopgfunctions;

/* Note: Need an empty line after each PG_CATALOG. function. See comment before PG_CATALOG.PG_TABLE_IS_VISIBLE for more detail. */
CREATE FUNCTION PG_CATALOG.OBJ_DESCRIPTION(INTEGER, VARCHAR) RETURNS VARCHAR AS $$pgObjDescription^%ydboctopgfunctions;

/* Note: Need an empty line after each PG_CATALOG. function. See comment before PG_CATALOG.PG_TABLE_IS_VISIBLE for more detail. */
CREATE FUNCTION PG_CATALOG.PG_BACKEND_PID() RETURNS VARCHAR AS $$pgBackendPid^%ydboctopgfunctions;

/* Note: Need an empty line after each PG_CATALOG. function. See comment before PG_CATALOG.PG_TABLE_IS_VISIBLE for more detail. */
CREATE FUNCTION PG_CATALOG.PG_GET_EXPR(VARCHAR, INTEGER) RETURNS VARCHAR AS $$pgGetExpr^%ydboctopgfunctions;

/* Until pg_database is fully implemented (#417) this will always return "SQL_ASCII" */
CREATE FUNCTION PG_ENCODING_TO_CHAR(INTEGER) RETURNS VARCHAR AS $$pgEncodingToChar^%ydboctopgfunctions;
/* Return true if database is currently recovering from a backup.
 * Since this feature is not implemented in Octo, this function will always return false.
 */
CREATE FUNCTION PG_IS_IN_RECOVERY() RETURNS BOOLEAN AS $$pgIsInRecover^%ydboctopgfunctions;
/* Return true if database recovery is currently paused.
 * Since this feature is not implemented in Octo, this function will always return false.
 */
CREATE FUNCTION PG_IS_XLOG_REPLAY_PAUSED() RETURNS BOOLEAN AS $$pgIsXlogReplayPaused^%ydboctopgfunctions;
/* Until pg_database is fully implemented (#417) this will always return "SQL_ASCII" */
/* Since Octo currently does not implement privileges, the following always return TRUE */
CREATE FUNCTION HAS_DATABASE_PRIVILEGE(INTEGER, VARCHAR) RETURNS BOOLEAN AS $$pgHasDatabasePrivilege^%ydboctopgfunctions;
CREATE FUNCTION HAS_DATABASE_PRIVILEGE(VARCHAR, VARCHAR, VARCHAR) RETURNS BOOLEAN AS $$pgHasDatabasePrivilege^%ydboctopgfunctions;

/* Stores PostgreSQL range types */
CREATE TABLE pg_catalog.pg_range (
	rngsubtype INTEGER,
	rngcollation INTEGER,
	rngsubopc INTEGER,
	rngcanonical INTEGER,
	rngsubdiff INTEGER,
	rngtypid INTEGER primary key
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_range"",keys(""rngtypid"")" READONLY;

/* Stores PostgreSQL enum types */
CREATE TABLE pg_catalog.pg_enum (
	enumtypid INTEGER,
	enumsortorder NUMERIC,
	enumlabel VARCHAR,
	oid INTEGER primary key
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_enum"",keys(""oid"")" READONLY;
