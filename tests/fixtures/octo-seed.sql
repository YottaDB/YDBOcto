#################################################################
#								#
# Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	#
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
CREATE TABLE octoOneRowTable (id INTEGER primary key) GLOBAL "^%ydboctoocto(""tables"",""octoOneRow"",keys(""id""))";

/* Used to store a list of 'namespaces'; basically everything we do should
 * fit in one of: public, pg_catalog, or information_schema
 */
CREATE TABLE pg_catalog.pg_namespace (
  nspname VARCHAR,
  nspowner INTEGER,
  nspacl VARCHAR,
  oid INTEGER primary key
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_namespace"",keys(""oid""))";

-- Note: Above GLOBAL is populated in `tests/fixtures/octo-seed.zwr` using the following query
--	select *,oid from pg_catalog.pg_type where typname in ('bool','int4','numeric','varchar','name');
-- And doing the following post-processing
--	a) Removing spaces from the output.
--	b) Replace `t` and `f` occurrences with `1` and `0` respectively.

CREATE TABLE pg_catalog.pg_type (
  typname VARCHAR(25) PRIMARY KEY PIECE "1",
  typnamespace INTEGER PIECE "2",
  typowner INTEGER PIECE "3",
  typlen INTEGER PIECE "4",
  typbyval BOOL PIECE "5",		-- specifying BOOL here just to test that works as good as BOOLEAN
  typtype VARCHAR(25) PIECE "6",
  typcategory VARCHAR(25) PIECE "7",
  typispreferred BOOLEAN PIECE "8",
  typisdefined BOOLEAN PIECE "9",
  typdelim VARCHAR(25) PIECE "10",
  typrelid INTEGER PIECE "11",
  typelem INTEGER PIECE "12",
  typarray INTEGER PIECE "13",
  typinput VARCHAR(25) PIECE "14",
  typoutput VARCHAR(25) PIECE "15",
  typreceive VARCHAR(25) PIECE "16",
  typsend VARCHAR(25) PIECE "17",
  typmodin VARCHAR(25) PIECE "18",
  typmodout VARCHAR(25) PIECE "19",
  typanalyze VARCHAR(25) PIECE "20",
  typalign VARCHAR(25) PIECE "21",
  typstorage VARCHAR(25) PIECE "22",
  typnotnull BOOLEAN PIECE "23",
  typbasetype INTEGER PIECE "24",
  typtypmod INTEGER PIECE "25",
  typndims INTEGER PIECE "26",
  typcollation INTEGER PIECE "27",
  typdefaultbin VARCHAR(25) PIECE "28",
  typdefault VARCHAR(25) PIECE "29",
  typacl VARCHAR(25) PIECE "30",
  oid INTEGER PIECE "31"
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_type"",keys(""typname""))";

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
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_class"",keys(""oid"")";

/* Populated via special DDL arguments */
CREATE TABLE pg_catalog.pg_description (
  objoid INTEGER,
  classoid INTEGER,
  objsubid INTEGER,
  description VARCHAR,
  oid INTEGER PRIMARY KEY
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_description"",keys(""oid"")";


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
) GLOBAL "^%ydboctoocto(""tables"",""information_schema"",""tables"",keys(""oid"")";


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
  proargtypes INTEGER,
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
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_proc"",keys(""oid"")";

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
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_attribute"",keys(""oid"")";

/* Stores default values for columns */
CREATE TABLE pg_catalog.pg_attrdef (
 adrelid INTEGER,
 adnum INTEGER,
 adbin VARCHAR,
 adsrc VARCHAR,
 oid INTEGER primary key
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_attrdef"",keys(""oid"")";

CREATE TABLE users (
  oid INTEGER,
  rolname VARCHAR KEY NUM "0",
  rolsuper INTEGER,
  rolinherit INTEGER,
  rolcreaterole INTEGER,
  rolcreatedb INTEGER,
  rolcanlogin INTEGER,
  rolreplication INTEGER,
  rolbypassrls INTEGER,
  rolconnlimit INTEGER,
  rolpassword VARCHAR,
  rolvaliduntil VARCHAR
);

/* Add standard SQL functions and internal Octo functions to the database and catalog
 * Note that some catalog functions do NOT use the types specified for them in PostgreSQL,
 * as Octo doesn't implement them (yet).
 * Note also that some functions are overloaded to account for various valid combinations of
 * parameter types, e.g. accept both `NUMERIC` and `INTEGER` types for the `ABS` function.
 */
CREATE FUNCTION ABS(NUMERIC) RETURNS NUMERIC AS $$ABS^%ydboctosqlfunctions;
CREATE FUNCTION ABS(INTEGER) RETURNS NUMERIC AS $$ABS^%ydboctosqlfunctions;
/* This only implements the 2-argument version of ROUND, since Octo doesn't support function overloading. */
CREATE FUNCTION ROUND(NUMERIC, INTEGER) RETURNS NUMERIC AS $$ROUND^%ydboctosqlfunctions;
CREATE FUNCTION ROUND(INTEGER, NUMERIC) RETURNS NUMERIC AS $$ROUND^%ydboctosqlfunctions;
CREATE FUNCTION ROUND(NUMERIC, NUMERIC) RETURNS NUMERIC AS $$ROUND^%ydboctosqlfunctions;
CREATE FUNCTION ROUND(INTEGER, INTEGER) RETURNS NUMERIC AS $$ROUND^%ydboctosqlfunctions;
/* This only implements the 2-argument version of TRUNC, since Octo doesn't support function overloading. */
CREATE FUNCTION TRUNC(NUMERIC, INTEGER) RETURNS NUMERIC AS $$TRUNC^%ydboctosqlfunctions;
CREATE FUNCTION TRUNC(INTEGER, NUMERIC) RETURNS NUMERIC AS $$TRUNC^%ydboctosqlfunctions;
CREATE FUNCTION TRUNC(NUMERIC, NUMERIC) RETURNS NUMERIC AS $$TRUNC^%ydboctosqlfunctions;
CREATE FUNCTION TRUNC(INTEGER, INTEGER) RETURNS NUMERIC AS $$TRUNC^%ydboctosqlfunctions;

/* REPLACE is used by SquirrelSQL during connection intialize and so is included here.
 * Note that REPLACE is not currently implemented and the matching M routine is an empty placeholder that
 * simply returns the first argument passed to it.
 */
CREATE FUNCTION REPLACE(VARCHAR, VARCHAR, VARCHAR) RETURNS VARCHAR AS $$^%ydboctoreplace;
CREATE FUNCTION ROW_NUMBER() RETURNS INTEGER AS $$^%ydboctopgRowNumber;
CREATE FUNCTION VERSION() RETURNS VARCHAR AS $$^%ydboctoversion;

CREATE FUNCTION CURRENT_SCHEMA() RETURNS VARCHAR AS $$^%ydboctocurrentSchema;
CREATE FUNCTION PG_CATALOG.CURRENT_SCHEMAS(BOOLEAN) RETURNS VARCHAR AS $$^%ydboctopgCurrentSchemas;
CREATE FUNCTION PG_CATALOG.OBJ_DESCRIPTION(INTEGER, VARCHAR) RETURNS VARCHAR AS $$^%ydboctopgObjDescription;
CREATE FUNCTION PG_CATALOG.PG_GET_EXPR(VARCHAR, INTEGER) RETURNS VARCHAR AS $$^%ydboctopgGetExpr;
