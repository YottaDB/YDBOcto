#################################################################
#								#
# Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

create table octoOneRowTable (id integer primary key) global "^%ydboctoocto(""tables"",""octoOneRow"",keys(""id""))";

CREATE TABLE pg_catalog.pg_namespace (
  oid integer primary key,
  nspowner integer,
  nspname varchar,
  nspacl varchar
) GLOBAL "^%ydboctoocto(""pg_catalog"",""pg_namespace"",keys(""oid""))";

CREATE TABLE pg_type (
  typname VARCHAR(25) PRIMARY KEY PIECE "1",
  typnamespace INTEGER PIECE "2",
  typowner INTEGER PIECE "3",
  typlen INTEGER PIECE "4",
  typbyval INTEGER PIECE "5",
  typtype VARCHAR(25) PIECE "6",
  typcategory VARCHAR(25) PIECE "7",
  typispreferred INTEGER PIECE "8",
  typisdefined INTEGER PIECE "9",
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
  typnotnull INTEGER PIECE "23",
  typbasetype INTEGER PIECE "24",
  typtypmod INTEGER PIECE "25",
  typndims INTEGER PIECE "26",
  typcollation INTEGER PIECE "27",
  typdefaultbin VARCHAR(25) PIECE "28",
  typdefault VARCHAR(25) PIECE "29",
  typacl VARCHAR(25) PIECE "30",
  oid INTEGER PIECE "31"
) GLOBAL "^%ydboctoocto(""pg_catalog"",""pg_type"",keys(""typname""))";

CREATE TABLE pg_catalog.pg_type (
  typname VARCHAR(25) PRIMARY KEY PIECE "1",
  typnamespace INTEGER PIECE "2",
  typowner INTEGER PIECE "3",
  typlen INTEGER PIECE "4",
  typbyval INTEGER PIECE "5",
  typtype VARCHAR(25) PIECE "6",
  typcategory VARCHAR(25) PIECE "7",
  typispreferred INTEGER PIECE "8",
  typisdefined INTEGER PIECE "9",
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
  typnotnull INTEGER PIECE "23",
  typbasetype INTEGER PIECE "24",
  typtypmod INTEGER PIECE "25",
  typndims INTEGER PIECE "26",
  typcollation INTEGER PIECE "27",
  typdefaultbin VARCHAR(25) PIECE "28",
  typdefault VARCHAR(25) PIECE "29",
  typacl VARCHAR(25) PIECE "30",
  oid INTEGER PIECE "31"
) GLOBAL "^%ydboctoocto(""pg_catalog"",""pg_type"",keys(""typname""))";


CREATE TABLE pg_catalog.pg_class (
  oid INTEGER PRIMARY KEY,
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
  relhasindex INTEGER,
  relisshared INTEGER,
  relpersistence VARCHAR,
  relkind VARCHAR,
  relnatts INTEGER,
  relchecks INTEGER,
  relhasoids INTEGER,
  relhaspkey INTEGER,
  relhasrules INTEGER,
  relhastriggers INTEGER,
  relhassubclass INTEGER,
  relrowsecurity INTEGER,
  relforcerowsecurity INTEGER,
  relispopulated INTEGER,
  relreplident VARCHAR,
  relispartition INTEGER,
  relfrozenxid INTEGER,
  relminmxid INTEGER,
  relacl VARCHAR,
  reloptions VARCHAR,
  relpartbound VARCHAR
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_class"",keys(""oid"")";

CREATE TABLE pg_catalog.pg_description (
  oid INTEGER PRIMARY KEY,
  objoid INTEGER,
  classoid INTEGER,
  objsubid INTEGER,
  description VARCHAR
) GLOBAL "^%ydboctoocto(""tables"",""pg_catalog"",""pg_description"",keys(""oid"")";


create table information_schema.tables (
  oid integer primary key,
  table_catalog varchar,
  table_schema varchar,
  table_name varchar,
  table_type varchar,
  self_referencing_column_name varchar,
  reference_generation varchar,
  user_defined_type_catalog varchar,
  user_defined_type_schema varchar,
  user_defined_type_name varchar,
  is_insertable_into varchar,
  is_typed varchar,
  commit_action varchar
) GLOBAL "^%ydboctoocto(""tables"",""information_schema"",""tables"",keys(""oid"")";


create table pg_catalog.pg_proc(
  oid integer primary key,
  proname varchar,
  pronamespace integer,
  proowner integer,
  prolang integer,
  procost integer,
  prorows integer,
  provariadic integer,
  protransform varchar,
  proisagg integer,
  proiswindow integer,
  prosecdef integer,
  proleakproof integer,
  proisstrict integer,
  proretset integer,
  provolatile varchar,
  proparallel varchar,
  pronargs integer,
  pronargdefaults integer,
  prorettype integer,
  proargtypes integer,
  proallargtypes integer,
  proargmodes varchar,
  proargnames varchar,
  proargdefaults varchar,
  protrftypes integer,
  prosrc varchar,
  probin varchar,
  proconfig varchar,
  proacl varchar
) GLOBAL "^%ydboctoocto(""tables"",""information_schema"",""tables"",keys(""oid"")";

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
)
