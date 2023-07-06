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

-- TC055 : OCTO583 : Validate IDENTITY constraint on columns
-- INSERT test cases
-- GENERATED ALWAYS AS IDENTITY
DROP TABLE IF EXISTS test;
CREATE TABLE test (id INT GENERATED ALWAYS AS IDENTITY, str TEXT);
  INSERT INTO test(str) VALUES('first'); -- Auto increment value assigned for ID
  SELECT * FROM test;
  -- OVERRIDING SYSTEM VALUE
  DROP TABLE IF EXISTS test;
  CREATE TABLE test (id INT GENERATED ALWAYS AS IDENTITY, str TEXT);
  INSERT INTO test(id,str) OVERRIDING SYSTEM VALUE VALUES(99,'first'); -- Auto increment value is ignore and 99 is assigned to ID
  SELECT * FROM test;
  INSERT INTO test OVERRIDING SYSTEM VALUE VALUES(100,'second'); -- Auto increment value is ignored and 100 is assigned to ID
  INSERT INTO test(str) VALUES('third'); -- ID value continues incrementing the stored initial value
  SELECT * FROM test;
  INSERT INTO test(str) VALUES('fourth');
  SELECT * FROM test;
DROP TABLE IF EXISTS test;

-- GENERATED BY DEFAULT
CREATE TABLE test (id INT GENERATED BY DEFAULT AS IDENTITY, str TEXT);
  INSERT INTO test(str) VALUES('first'); -- no explicit value for ID specified so auto-increment value is assigned
  SELECT * FROM test;
  INSERT INTO test VALUES(3,'second'); -- explicit value for ID specified so auto-increment value ignored
  SELECT * FROM test;
  INSERT INTO test(str) VALUES('third'); -- no explicit value for ID specified so auto-increment value is assigned
  SELECT * FROM test;
  INSERT INTO test VALUES(999,'fourth'); -- explicit value for ID specified so auto-increment value ignored
  SELECT * FROM test;
  INSERT INTO test(str) VALUES('fifth'); -- no explicit value for ID specified so auto-increment value is assigned
  SELECT * FROM test;
  INSERT INTO test(str) VALUES('sixth'); -- no explicit value for ID specified so auto-increment value is assigned
  SELECT * FROM test;
DROP TABLE IF EXISTS test;

-- UPDATE test cases
-- GENERATED ALWAYS AS IDENTITY
CREATE TABLE test (id INT GENERATED ALWAYS AS IDENTITY, str TEXT);
  UPDATE test SET str=str||'x'; -- 0 rows are updated
  SELECT * FROM test;
  INSERT INTO test(str) values('first');
  SELECT * FROM test;
  UPDATE test SET str=str||'x'; -- 1 row updated but no change to ID column
  SELECT * FROM test;
  UPDATE test SET id=DEFAULT,str='first'; -- The changed rows will have auto-increment value assigned to ID
DROP TABLE IF EXISTS test;

-- GENERATED BY DEFAULT AS IDENTITY
CREATE TABLE test (id INT GENERATED BY DEFAULT AS IDENTITY, str TEXT);
  UPDATE test SET id=1; -- 0 rows will be updated
  UPDATE test SET id=1,str='first'; -- 0 rows will be updated
  UPDATE test SET str=str||'x'; -- 0 rows will be updated
  SELECT * FROM test;
  INSERT INTO test(str) values('first');
  SELECT * FROM test;
  UPDATE test SET id=99; -- 1 row updated but no change to ID column
  SELECT * FROM test;
  UPDATE test SET str=str||'x'; -- 1 row updated but no change to ID column
  SELECT * FROM test;
  UPDATE test SET id=DEFAULT,str='first'; -- The changed rows will have auto-increment value assigned to ID
  DROP TABLE IF EXISTS test;

-- PRIMARY KEY which is an IDENTITY
CREATE TABLE test (id INT PRIMARY KEY GENERATED ALWAYS AS IDENTITY, str TEXT);
  INSERT INTO test(str) VALUES('first'); -- auto-increment value is assigned to ID
  SELECT * FROM test;
DROP TABLE IF EXISTS test;

-- Copying data from a table when destination table has identity columns
CREATE TABLE test (id INT GENERATED ALWAYS AS IDENTITY, str TEXT);
INSERT INTO test(str) VALUES('second');
SELECT * FROM test;
DROP TABLE IF EXISTS test;
CREATE TABLE test (id INT GENERATED BY DEFAULT AS IDENTITY, str TEXT);
SELECT * FROM test;
INSERT INTO test(str) VALUES('second');
SELECT * FROM test;
DROP TABLE IF EXISTS test;

-- IDENTITY on different INTEGER columns
CREATE TABLE test (id SMALLINT GENERATED ALWAYS AS IDENTITY, name TEXT);
DROP TABLE IF EXISTS test;
CREATE TABLE test (id BIGINT GENERATED ALWAYS AS IDENTITY, name TEXT);
DROP TABLE IF EXISTS test;
-- Following usage of INT(2) not recognized by Postgres
-- CREATE TABLE test (id INT(2) GENERATED ALWAYS AS IDENTITY, name TEXT);
-- DROP TABLE IF EXISTS test;
CREATE TABLE test (id INT2 GENERATED ALWAYS AS IDENTITY, name TEXT);
DROP TABLE IF EXISTS test;
CREATE TABLE test (id INT4 GENERATED ALWAYS AS IDENTITY, name TEXT);
DROP TABLE IF EXISTS test;
CREATE TABLE test (id INT8 GENERATED ALWAYS AS IDENTITY, name TEXT);
DROP TABLE IF EXISTS test;

CREATE TABLE test (id SMALLINT GENERATED BY DEFAULT AS IDENTITY, name TEXT);
DROP TABLE IF EXISTS test;
CREATE TABLE test (id BIGINT GENERATED BY DEFAULT AS IDENTITY, name TEXT);
DROP TABLE IF EXISTS test;
-- Following usage of INT(2) not recognized by Postgres
-- CREATE TABLE test (id INT(2) GENERATED BY DEFAULT AS IDENTITY, name TEXT);
-- DROP TABLE IF EXISTS test;
CREATE TABLE test (id INT2 GENERATED BY DEFAULT AS IDENTITY, name TEXT);
DROP TABLE IF EXISTS test;
CREATE TABLE test (id INT4 GENERATED BY DEFAULT AS IDENTITY, name TEXT);
DROP TABLE IF EXISTS test;
CREATE TABLE test (id INT8 GENERATED BY DEFAULT AS IDENTITY, name TEXT);
DROP TABLE IF EXISTS test;


-- OVERRIDING USER VALUE working with GENERATED BY DEFAULT AS IDENTITY
create table test (id int generated by default as identity, name text);

-- User input for identity column is ignored
insert into test(id,name) overriding user value values(5,'first');
select * from test;

-- User input for identity column is ignored
insert into test overriding user value values(5,'second');
select * from test;
DROP TABLE IF EXISTS test;

-- DEFAULT set value for IDENTITY column in an UPDATE query
create table test (id int generated always as identity, name text);
update test set id=default;
insert into test(name) values('first');
update test set id=default;
update test set id=default;
select * from test; -- Represents id value of 3 (1 from insert, 1 from update and another from update)
DROP TABLE IF EXISTS test;

-- INSERT with OVERRIDING USER VALUE on a GENERATED BY DEFAULT AS IDENTITY column
create table test (id int generated by default as identity, str text);
insert into test(str) overriding user value values ('abcd');
select * from test;
drop table if exists test;

-- INSERT with OVERRIDING SYSTEM VALUE on a GENERATED ALWAYS AS IDENTITY column without specifying a value for the column
create table test (id integer generated always as identity, name text);
insert into test(name) overriding system value values('first');
insert into test(name) overriding system value values('first');
select * from test;
drop table if exists test;

-- Spaces between IDENTITY related keywords should be allowed
create table test (id integer generated   always as         identity, names text);
insert into test overriding     system    value    values(145,'first');
select * from test;
drop table if exists test1;
create table test1(id integer     generated by      default     as identity, names text);
insert into test1   overriding user   value values(99,'first');
select * from test1;
drop table test;
drop table test1;
drop table if exists test;
create table test (id int generated by	default  as	 identity, str text);

drop table if exists test;
create table test (id int generated
by	default  as	 identity, str text);

drop table if exists test;
create table test (id int generated
by	default
as
		identity, str text);
drop table if exists test;
