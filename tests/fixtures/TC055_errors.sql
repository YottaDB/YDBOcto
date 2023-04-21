#################################################################
#								#
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TC055 : OCTO583 : Validate IDENTIY constraint on columns
-- INSERT
-- GENERATED ALWAYS AS IDENTITY
DROP TABLE IF EXISTS test;
CREATE TABLE test (id INT GENERATED ALWAYS AS IDENTITY, str TEXT);
  INSERT INTO test VALUES('first'); -- ERROR: type missmatch integer and varchar in Postgres but IDENTITY error in Octo because of order in which error is checked
  INSERT INTO test VALUES(3,'second'); -- ERROR cannot insert into GENERATED ALWAYS type of column (hint: use OVERRIDE with INSERT)
  INSERT INTO test(id,str) VALUES(3,'second'); -- ERROR cannot insert into GENERATED ALWAYS type of column (hint: use OVERRIDE with INSERT)
  INSERT INTO test(id,str) SELECT 1,'third' UNION SELECT 2,'fourth'; -- ERROR cannot insert into GENERATED ALWAYS type of column (hint: use OVERRIDE with INSERT)
  INSERT INTO test SELECT 1,'third' UNION SELECT 2,'fourth'; -- ERROR cannot insert into GENERATED ALWAYS type of column (hint: use OVERRIDE with INSERT)
  SELECT * FROM test;
  -- OVERRIDING SYSTEM VALUE
  DROP TABLE IF EXISTS test;
  CREATE TABLE test (id INT GENERATED ALWAYS AS IDENTITY, str TEXT);
  INSERT INTO test VALUES(99,'fifth'); -- EROR cannot insert into GENERATED ALWAYS type of column (hint: use OVERRIDE with INSERT)
  SELECT * FROM test;
DROP TABLE IF EXISTS test;

-- GENERATED BY DEFAULT
CREATE TABLE test (id INT GENERATED BY DEFAULT AS IDENTITY, str TEXT);
  INSERT INTO test VALUES('first'); -- ERROR ERR_INSERT_TYPE_MISMATCH
DROP TABLE IF EXISTS test;

-- UPDATE
-- GENERATED ALWAYS AS IDENTITY
CREATE TABLE test (id INT GENERATED ALWAYS AS IDENTITY, str TEXT);
  UPDATE test SET id=1; -- ERROR column "id" can only be updated to DEFAULT (ERR_UPDATE_OF_GENERATED_ALWAYS_IDENTITY)
  UPDATE test SET id=1,str='first'; -- ERROR column "id" can only be updated to DEFAULT (ERR_UPDATE_OF_GENERATED_ALWAYS_IDENTITY)
  UPDATE test SET id=1; -- ERROR column "id" can only be updated to DEFAULT (ERR_UPDATE_OF_GENERATED_ALWAYS_IDENTITY)
DROP TABLE IF EXISTS test;

-- PRIMARY KEY
CREATE TABLE test (id INT PRIMARY KEY GENERATED ALWAYS AS IDENTITY, str TEXT);
  INSERT INTO test VALUES(1,'second'); -- IDENTITY column error (ERR_INSERT_ON_GENERATED_ALWAYS_IDENTITY)
  INSERT INTO test OVERRIDING SYSTEM VALUE VALUES(1,'second'); -- ERR_DUPLICATE_KEY_VALUE error
DROP TABLE IF EXISTS test;

-- IDENTITY on ready-only table
CREATE TABLE test (id INT GENERATED ALWAYS AS IDENTITY, str TEXT) READONLY; -- ERR_READONLY_DISALLOWED error

-- Implicit read-only table with IDENTITY
CREATE TABLE namesE(id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,firstName VARCHAR EXTRACT "^names(keys(""ID""))",lastName VARCHAR) GLOBAL "^names(keys(""ID""))"; -- ERR_READONLY_AND_READWRITE_DISALLOWED

-- Copying data from a table when destination table has identity columns
CREATE TABLE test (id INT GENERATED ALWAYS AS IDENTITY, str TEXT);
INSERT INTO test SELECT id, lastname FROM names; -- IDENTITY column error (ERR_INSERT_ON_GENERATED_ALWAYS_IDENTITY)
INSERT INTO test OVERRIDING SYSTEM VALUE VALUES(NULL,'first'); -- ERR_NULL_COL_VALUE for id
INSERT INTO test SELECT id, lastname FROM names; -- ERR_INSERT_ON_GENERATED_ALWAYS_IDENTITY
SELECT * FROM test;
INSERT INTO test VALUES(NULL,'first'); -- ERR_INSERT_ON_GENERATED_ALWAYS_IDENTITY
SELECT * FROM test;
DROP TABLE IF EXISTS test;

-- IDENTITY on non-integer columns (ERR_NON_INTEGER_IDENTITY error is expected)
CREATE TABLE test (name TEXT GENERATED ALWAYS AS IDENTITY, id INT); -- Error IDENTITY column can only be on smallint, integer or bigint
CREATE TABLE test (name VARCHAR(20) GENERATED ALWAYS AS IDENTITY, id INT); -- Error IDENTITY column can only be on smallint, integer or bigint
CREATE TABLE test (name NUMERIC GENERATED ALWAYS AS IDENTITY, id INT); -- Error IDENTITY column can only be on smallint, integer or bigint
CREATE TABLE test (name NUMERIC(20,2) GENERATED ALWAYS AS IDENTITY, id INT); -- Error IDENTITY column can only be on smallint, integer or bigint
CREATE TABLE test (name BOOLEAN GENERATED ALWAYS AS IDENTITY, id INT); -- Error IDENTITY column can only be on smallint, integer or bigint

CREATE TABLE test (name TEXT GENERATED BY DEFAULT AS IDENTITY, id INT); -- Error IDENTITY column can only be on smallint, integer or bigint
CREATE TABLE test (name VARCHAR(20) GENERATED BY DEFAULT AS IDENTITY, id INT); -- Error IDENTITY column can only be on smallint, integer or bigint
CREATE TABLE test (name NUMERIC GENERATED BY DEFAULT AS IDENTITY, id INT); -- Error IDENTITY column can only be on smallint, integer or bigint
CREATE TABLE test (name NUMERIC(20,2) GENERATED BY DEFAULT AS IDENTITY, id INT); -- Error IDENTITY column can only be on smallint, integer or bigint
CREATE TABLE test (name BOOLEAN GENERATED BY DEFAULT AS IDENTITY, id INT); -- Error IDENTITY column can only be on smallint, integer or bigint

-- OVERRIDING USER VALUE working with GENERATED ALWAYS AS IDENTITY (Error ERR_INSERT_ON_GENERATED_ALWAYS_IDENTITY)
create table test (id int generated always as identity, name text);
insert into test(id,name) overriding user value values(5,'first'); -- ERROR Cannot insert, column `id` defined as GENERATED ALWAYS IDENTITY
insert into test overriding user value values(5,'second'); -- ERROR Cannot insert, column `id` defined as GENERATED ALWAYS IDENTITY
DROP TABLE IF EXISTS test;

-- DEFAULT set value for non-IDENTITY column in an UPDATE query is not implemented. Such usage should result in
-- ERR_FEATURE_NOT_IMPLEMENTED error.
-- Note:
-- * test_createtable.bats.in includes READONLY as table type in octo.conf. This results in the table `test` being considered as a READONLY
--   table. In this case, `UPDATE` that follows will lead to a `ERR_TABLE_READONLY` error and thats not what we are testing. Hence the use of
--   READWRITE keyword to allow `UPDATE` usage on the table.
create table test (id int, name text) READWRITE;
update test set id=default;
DROP TABLE IF EXISTS test;

-- Test that ERR_TABLE_MULTIPLE_IDENTITY message is seen when mutiple identity keywords are included for a column
create table test (id int generated always as identity generated by default as identity, name text);
create table test (id int generated always as identity generated by default as identity generated by default as identity, name text);
create table test (id int generated by default as identity generated by default as identity generated by default as identity, name text);
create table test (name text, id int generated always as identity generated by default as identity);

-- Double-quoted identifiers yield correct error output in ERR_UPDATE_OF_GENERATED_ALWAYS_IDENTITY and ERR_INSERT_ON_GENERATED_ALWAYS_IDENTITY
create table "T M P" ("i d" int generated always as identity, str text);
update "T M P" set "i d"=1; -- ERROR Cannot update, column `"i d"` defined as GENERATED ALWAYS IDENTITY
insert into "T M P"("i d",name) overriding user value values(5,'first'); -- ERROR Cannot insert, column `"i d"` defined as GENERATED ALWAYS IDENTITY
