#################################################################
#								#
# Copyright (c) 2021-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TCTC003 : OCTO772 : Test various errors in CONSTRAINTS as well as a few valid (i.e. non-error) cases

-- Test table level NOT NULL constraint issues SYNTAX ERROR
create table products (first varchar, last varchar, not null);

-- Test column level UNIQUE constraint issues SYNTAX ERROR if one or more columns is specified along with it.
-- Only a table level UNIQUE constraint is allowed to specify one or more columns.
create table products (id integer, name varchar unique (name));
create table products (id integer, name varchar unique (id, name));

-- Test of ERR_TABLE_MULTIPLE_PRIMARY_KEYS error
create table products (id integer constraint prikey primary key primary key);
create table products (id integer constraint prikey primary key constraint seckey primary key);
create table products (id integer constraint prikey primary key, name varchar primary key);
create table products (id integer constraint prikey primary key, name varchar, primary key (id, name));
create table products (id1 integer primary key, id2 integer key num 1, primary key (id1, id2));
create table products (id1 integer, id2 integer, primary key (id1, id2), primary key (id2, id1));
-- Tests of ERR_TABLE_MULTIPLE_PRIMARY_KEYS error that demonstrate the need for "primary_key_constraint_keyword" variable
-- in "src/parser/table_definition.c" (i.e. just "primary_key_constraint_col" is not enough).
create table tmp103 (constraint pkey103 primary key (id1), primary key (id1), id1 integer);
create table tmp104 (id1 integer, constraint pkey104 primary key (id1), primary key (id1));
create table tmp105 (constraint pkey105 primary key (id1), id1 integer, primary key (id1));

-- Test of ERR_DUPLICATE_PRIMARY_KEY_CONSTRAINT error
create table tmp1 (id1 integer constraint tmp_pkey primary key);
create table tmp2 (id2 integer constraint tmp_pkey primary key);
drop table tmp1;

-- Test of ERR_AGGREGATE_FUNCTION_CHECK error
create table products (id integer, name text CHECK (COUNT(*) is NULL));
create table products (id integer, name text CHECK (COUNT(name) is NULL));
create table products (id integer, name text CHECK (MIN(name) is NULL));
create table products (id integer, name text CHECK (MAX(name) is NULL));
create table products (id integer, name text CHECK (AVG(name) is NULL));
create table products (id integer, name text CHECK (SUM(name) is NULL));

-- Test of ERR_SUBQUERY_CHECK error
create table products (product_no integer, name text, price numeric CONSTRAINT name1 CHECK (price < (select 1000)));
create table products (product_no integer, name text, price numeric CONSTRAINT name1 CHECK (price < (select * from names)));

-- Test of ERR_AGGREGATE_FUNCTION_CHECK error (ERR_SUBQUERY_CHECK error exists too but will not show since it is 2nd error)
create table products (id integer, name text CHECK (COUNT(*) = (SELECT 1000)));

-- Test of ERR_UNKNOWN_TABLE error in CHECK constraint
create table products (product_no integer, name text, price numeric CONSTRAINT name1 CHECK (price < (select * from invalid)));

-- Test of syntax error (TABLE_ASTERISK case in src/qualify_check_constraint.c)
create table products (product_no integer, name text, price numeric CONSTRAINT name1 CHECK (price < *));
create table products (product_no integer, name text, price numeric CONSTRAINT name1 CHECK (price < t.*));
create table products (product_no integer, name text, price numeric CONSTRAINT name1 CHECK (price < products.*));
create table products (product_no integer, name text, price numeric CONSTRAINT name1 CHECK (price in products.*));
create table products (product_no integer, name text, price numeric CONSTRAINT name1 CHECK (price in names.*));
create table products (product_no integer, name text, price numeric CONSTRAINT name1 CHECK (price < names.*));

-- Test of ERR_MISSING_FROM_ENTRY error in CHECK constraint
-- Test existing table name "names"
create table products (product_no integer, name text, price numeric CONSTRAINT name1 CHECK (price < names.id));
-- Test non-existent table name "x"
create table products (id integer, name varchar CHECK (2 > 0) CHECK (x.name is not null));

-- Test of ERR_UNKNOWN_COLUMN_NAME error in CHECK constraint
create table products (product_no integer, name text, price numeric CONSTRAINT name1 CHECK (price < invalid));
create table products (product_no integer, name text, price numeric CONSTRAINT name1 CHECK (price < product));
---- Test error inside function call parameters
create table products (product_no integer, name text, price numeric CONSTRAINT name1 CHECK (price < samevalue(product)));
create table products (product_no integer, name text, price numeric CONSTRAINT name1 CHECK (ABS(id)));
create table products (product_no integer, price numeric CONSTRAINT name1 CHECK (price < COALESCE(NULL, 1, invalidcol)));
create table products (product_no integer, price numeric CONSTRAINT name1 CHECK (price < GREATEST(1, invalidcol)));
create table products (product_no integer, price numeric CONSTRAINT name1 CHECK (price < LEAST(1, invalidcol)));
create table products (product_no integer, price numeric CONSTRAINT name1 CHECK (price < NULLIF(1, invalidcol)));

-- Test of syntax error in CHECK constraint (missing surrounding parentheses for entire expression)
create table products (product_no integer, name text, price numeric CONSTRAINT name1 CHECK (price > 5) AND (price < 10));

-- Test of ERR_TYPE_NOT_COMPATIBLE error
create table products (id integer CHECK (id + firstname), firstname varchar);
create table products (id integer CHECK (id - true), firstname varchar);
create table products (id integer CHECK (id * 0::boolean), firstname varchar);
create table products (id integer CHECK (id / 1::varchar), firstname varchar);
create table products (id integer CHECK (id % true), firstname varchar);
create table products (id integer CHECK (id || true), firstname varchar);
create table products (id integer CHECK (id || 0::numeric), firstname varchar);
create table products (id integer CHECK (NULL || 0::numeric), firstname varchar);
create table products (id integer CHECK (firstname), firstname varchar);
create table products (id integer CHECK (firstname || id), firstname varchar);
create table products (id integer CHECK (id::boolean || firstname), firstname varchar);
create table products (id integer CHECK (firstname AND id::boolean), firstname varchar);
create table products (id integer CHECK (id::boolean OR id), firstname varchar);
---- Test error inside function call parameters
create table products (product_no integer, name text, price numeric CONSTRAINT name1 CHECK (ABS(product_no)));

-- Test of ERR_TYPE_MISMATCH error
create table products (id integer CHECK (firstname IN (1, 2)), firstname varchar);
create table products (id integer CHECK (id IN (1, 'abcd')), firstname varchar);
create table products (id integer CHECK (COALESCE(NULL, 'a', 1.2)), firstname varchar);
create table products (id integer CHECK (COALESCE(id, NULL, firstname)), firstname varchar);
create table products (id integer CHECK (GREATEST(NULL, 'a', 1.2)), firstname varchar);
create table products (id integer CHECK (GREATEST(id, NULL, firstname)), firstname varchar);
create table products (id integer CHECK (LEAST(NULL, 'a', 1.2)), firstname varchar);
create table products (id integer CHECK (LEAST(id, NULL, firstname)), firstname varchar);
create table products (id integer CHECK (NULLIF('a', 1.2)), firstname varchar);
create table products (id integer CHECK (NULLIF(id, firstname)), firstname varchar);

-- Test of ERR_INVALID_INPUT_SYTNAX error
create table products (id integer CHECK (+id::boolean));
create table products (id integer CHECK (-id::boolean));
create table products (id integer CHECK (-id::varchar));
create table products (id integer, firstname varchar CHECK (firstname = +'Zero'));
create table products (id integer, firstname varchar CHECK (firstname = 'Zero' || +'Zero'));
create table products (id integer, firstname varchar CHECK (firstname = 'Zero' || -'Zero'));
create table products (id integer, firstname varchar CHECK (firstname = 'Zero' || -NULL));
 -- +NULL below is a valid usage. Drop the table created by the below command so that other tests are not affected
create table products (id integer, firstname varchar CHECK (firstname = 'Zero' || +NULL));
drop table products;

-- Test of ERR_UNKNOWN_FUNCTION error
create table products (product_no integer, name text, price numeric CONSTRAINT name1 CHECK (ABS(name)));
create table products (product_no integer, name text, price numeric CONSTRAINT name1 CHECK (INVALIDFUNC(name)));
create table products (product_no integer, name text, price numeric CONSTRAINT name1 CHECK (price * samevalue(product_no) < 1000));

-- Test of ERR_TOO_MANY_FUNCTION_ARGUMENTS error
create table products (product_no integer, name text, price numeric CONSTRAINT name1 CHECK (INVALIDFUNC(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33)));

-- Test of ERR_CASE_BRANCH_TYPE_MISMATCH error
create table products (id integer CONSTRAINT name1 CHECK (case when 1=1 then 2 when 2=2 then 'abcd' end));
create table products (id integer CONSTRAINT name1 CHECK (case when 1=1 then 2 else 'abcd' end));
create table products (id integer CONSTRAINT name1 CHECK (case id when 1 then 'abcd' when 2 then id end));
create table products (id integer CONSTRAINT name1 CHECK (case id when 1 then 'abcd' else id end));

-- Test of ERR_CASE_VALUE_TYPE_MISMATCH error
create table products (id integer CONSTRAINT name1 CHECK (case when id then 2 end));
create table products (id integer CONSTRAINT name1 CHECK (case id when 'abcd' then 2 end));

-- Test of ERR_DUPLICATE_CONSTRAINT error
-- Test explicitly specified constraint name collision within multiple CHECK constraints all inside one column
CREATE TABLE products (
	     product_no integer,
	     name text,
	     price numeric CHECK (price > 0) CONSTRAINT name1 CHECK (price > 5) CONSTRAINT name1 CHECK (price < 10)
	 );
-- Test explicitly specified constraint name collision across CHECK constraints in multiple columns
CREATE TABLE products (
	     product_no integer CONSTRAINT name1 CHECK (product_no > 0),
	     name text CONSTRAINT name2 CHECK (name is NOT NULL),
	     price numeric CONSTRAINT name2 CHECK (price > 5)
	 );
-- Test explicitly specified constraint name collision between a column level and table level CHECK constraint
CREATE TABLE products (
	     product_no integer,
	     name text,
	     price numeric CHECK (price > 0) CONSTRAINT name3 CHECK (price > 5),
	     CONSTRAINT name3 CHECK (price < 10)
	 );
-- Test explicitly specified constraint name collision between two table level CHECK constraints
CREATE TABLE products (
	     product_no integer,
	     name text,
	     price numeric CHECK (price > 0),
	     CONSTRAINT name3 CHECK (price > 5),
	     CONSTRAINT name3 CHECK (price < 10)
	 );
-- Test explicitly specified constraint name that collides with a previously specified implicitly assigned CHECK constraint
CREATE TABLE products (
	     product_no integer CHECK (product_no > 0),
	     name text,
	     price numeric CONSTRAINT products_product_no_check CHECK (price > 5)
	 );
-- Test name collision between implicitly assigned constraint name and explicitly specified constraint name in same column
CREATE TABLE products (
	     product_no integer,
	     name text,
	     price numeric CHECK (price > 0) CHECK (price > 5) CONSTRAINT products_price_check1 CHECK (price < 10)
	 );
-- Test name collision among multiple 63-byte user specified column-level constraint names (longest allowed name length)
CREATE TABLE products (
	     product_no integer
	     CONSTRAINT toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqrstu CHECK (product_no < 2)
	     CONSTRAINT toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqrstu CHECK (product_no > 0)
	);
-- Test name collision among multiple 63-byte user specified table-level constraint names (longest allowed name length)
CREATE TABLE products (
	     product_no integer,
	     CONSTRAINT toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqrstu CHECK (product_no < 2),
	     CONSTRAINT toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqrstu CHECK (product_no > 0)
	);

-- Test of ERR_IDENT_LENGTH error on CHECK constraint name by specifying a 64-byte name (max allowed is 63 bytes)
CREATE TABLE products (
	product_no integer CONSTRAINT toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqrstuv CHECK (product_no < 2));

-- Test of ERR_IDENT_LENGTH error on UNIQUE constraint name by specifying a 64-byte name (max allowed is 63 bytes)
CREATE TABLE products (product_no integer CONSTRAINT toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqrstuv UNIQUE);

-- Test of ERR_TABLE_MUST_HAVE_A_VISIBLE_COLUMN error
CREATE TABLE products (CHECK (1 > 0));
CREATE TABLE products (CHECK (1 > 0), CHECK (2 > 1));

-- Test of ERR_CHECK_CONSTRAINT_VIOLATION error
-- Test of 1 CHECK column constraint on non-key column
CREATE TABLE products (product_no integer, name text, price numeric CHECK (price > 0));
INSERT INTO products VALUES (1, 'abcd', 5);
INSERT INTO products VALUES (2, 'efgh', -1);
INSERT INTO products VALUES (3, NULL, 6);
UPDATE products SET price = -1;
-- Also test NULL column values (NULL value in "name" column) get displayed correctly in ERR_CHECK_CONSTRAINT_VIOLATION error
UPDATE products SET price = -1 WHERE product_no = 3;
SELECT * from products;
DROP TABLE products;

-- Test of ERR_CHECK_CONSTRAINT_VIOLATION error
-- Test of multiple CHECK column constraints on non-key column
CREATE TABLE products (product_no integer, name text, price numeric CHECK (price > 0) CHECK (price > 5));
INSERT INTO products VALUES (1, 'abcd', 6);
INSERT INTO products VALUES (2, 'efgh', 5);
UPDATE products SET price = 5;
SELECT * from products;
DROP TABLE products;

-- Test of ERR_CHECK_CONSTRAINT_VIOLATION error
-- Test of 1 CHECK column constraint on primary key column
CREATE TABLE products (product_no integer PRIMARY KEY CHECK (product_no > 2), name text);
INSERT INTO products VALUES (3, 'abcd');
INSERT INTO products VALUES (2, 'efgh');
UPDATE products SET product_no = 2;
SELECT * from products;
DROP TABLE products;

-- Test of ERR_CHECK_CONSTRAINT_VIOLATION error
-- Test of multiple CHECK column constraints on primary key column
CREATE TABLE products (product_no integer PRIMARY KEY CHECK (product_no > 2) CHECK (product_no < 10), name text);
INSERT INTO products VALUES (5, 'abcd');
INSERT INTO products VALUES (10, 'efgh');
UPDATE products SET product_no = 10;
SELECT * from products;
DROP TABLE products;

-- Test of ERR_CHECK_CONSTRAINT_VIOLATION error
-- Test of complex column-level CHECK constraint
CREATE TABLE products (
             product_no integer,
             name text,
             price numeric CONSTRAINT name1
                CHECK (((price > 5) AND (price < 8)) OR (price = 0) OR (price is NULL) OR (price * price < 100))
        );
INSERT INTO products VALUES (1, 'abcd', 9);
INSERT INTO products VALUES (2, 'efgh', 10);
INSERT INTO products VALUES (3, 'ijkl', NULL);
UPDATE products SET price = 10;
SELECT * from products;
DROP TABLE products;

-- Test of ERR_CHECK_CONSTRAINT_VIOLATION error
-- Test of complex table-level CHECK constraint
CREATE TABLE products (
             product_no integer,
             name text,
             price numeric,
             CHECK ((price is NULL) OR (product_no is NULL) OR ((price * product_no) <= 2000))
        );
INSERT INTO products VALUES (1, 'abcd', 2000);
INSERT INTO products VALUES (2, 'efgh', 1001);
INSERT INTO products VALUES (3, 'ijkl', NULL);
UPDATE products SET price = 1001;
SELECT * from products;
DROP TABLE products;

-- Test that check constraint is satisfied if it evalutes to NULL (not just TRUE)
CREATE TABLE products (product_no integer, name text CHECK (product_no < 5));
INSERT INTO products VALUES (4, 'abcd');
INSERT INTO products VALUES (5, 'efgh');
INSERT INTO products VALUES (NULL, 'ijkl');
UPDATE products SET product_no = NULL;
SELECT * from products;
DROP TABLE products;

-- Test of ERR_CHECK_CONSTRAINT_VIOLATION error
-- Test of multiple CHECK column constraints on key and non-key columns
-- Also test that table-level constraints can be in between column-level constraints (i.e. order does not matter).
CREATE TABLE products (
                product_no integer PRIMARY KEY CHECK (product_no > 2) CHECK (product_no < 10),
                CHECK ((name || product_no) < 'Lord8'),
                name text CHECK (name > 'Cereal') CHECK (name < 'Zero')
        );
INSERT INTO products VALUES (2, 'Str2');
INSERT INTO products VALUES (9, 'Lord3');
INSERT INTO products VALUES (10, 'Str10');
INSERT INTO products VALUES (4, 'Cereal');
INSERT INTO products VALUES (5, 'Zero');
INSERT INTO products VALUES (6, 'Joey');
INSERT INTO products VALUES (8, 'Lord');
UPDATE products SET name = 'Str2';
UPDATE products SET name = 'Str10';
UPDATE products SET name = 'Cereal';
UPDATE products SET name = 'Zero';
UPDATE products SET name = 'Lord';
SELECT * from products;
DROP TABLE products;

-- Test CONSTRAINT treates '' usages as NULL (Octo is like Oracle and differs from Postgres and MySQL in this regard)
-- and so `!= ''` checks do not work as intended (Test (a) below) and need to be `IS NOT NULL` instead (Test (b) below).
-- See https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1134#note_1016095301 for more details

-- Test (a) : That `!= ''` does not work as intended. A ERR_CHECK_CONSTRAINT_VIOLATION error is expected but does not show up.
CREATE TABLE users(
    id              integer primary key,
    first_name      varchar(50)        ,
    last_name       varchar(50)        ,
    nick_name       varchar(100)       ,
    email           varchar(255)       ,
    password_digest text               ,
    created_at      integer            ,
    updated_at      integer            ,
    CONSTRAINT validate_name CHECK((first_name != '' AND last_name != '') OR nick_name != '')
);
INSERT INTO users(id, first_name, last_name, nick_name, email, password_digest)
VALUES
(1, 'Ms.', 'Winter', 'ms..winter', 'ms..winter@mailinator.com', 'c75029a15cbf675cf06b98ce7defeaa3'),
(2, 'Sebastian', 'Batz', 'sebastian.batz', 'sebastian.batz@mailinator.com', '2518b96a5669f2fc884c8be0f7668feb'),
(3, '', '', '', 'ms..winter@mailinator.com', 'c75029a15cbf675cf06b98ce7defeaa3'),
(4, '', '', '', 'sebastian.batz@mailinator.com', '2518b96a5669f2fc884c8be0f7668feb');
DROP TABLE IF EXISTS users;

-- Test (b) : That `IS NOT NULL` works as intended. A ERR_CHECK_CONSTRAINT_VIOLATION error is expected and does show up.
CREATE TABLE users(
    id              integer primary key,
    first_name      varchar(50)        ,
    last_name       varchar(50)        ,
    nick_name       varchar(100)       ,
    email           varchar(255)       ,
    password_digest text               ,
    created_at      integer            ,
    updated_at      integer            ,
    CONSTRAINT validate_name CHECK((first_name IS NOT NULL AND last_name IS NOT NULL) OR nick_name IS NOT NULL)
);
INSERT INTO users(id, first_name, last_name, nick_name, email, password_digest)
VALUES
(1, 'Ms.', 'Winter', 'ms..winter', 'ms..winter@mailinator.com', 'c75029a15cbf675cf06b98ce7defeaa3'),
(2, 'Sebastian', 'Batz', 'sebastian.batz', 'sebastian.batz@mailinator.com', '2518b96a5669f2fc884c8be0f7668feb'),
(3, '', '', '', 'ms..winter@mailinator.com', 'c75029a15cbf675cf06b98ce7defeaa3'),
(4, '', '', '', 'sebastian.batz@mailinator.com', '2518b96a5669f2fc884c8be0f7668feb');
INSERT INTO users(id, first_name, last_name, nick_name, email, password_digest)
VALUES
(1, 'Ms.', 'Winter', 'ms..winter', 'ms..winter@mailinator.com', 'c75029a15cbf675cf06b98ce7defeaa3'),
(2, 'Sebastian', 'Batz', 'sebastian.batz', 'sebastian.batz@mailinator.com', '2518b96a5669f2fc884c8be0f7668feb');
UPDATE users SET first_name = NULL, nick_name = NULL;
UPDATE users SET last_name = NULL, nick_name = NULL;
UPDATE users SET first_name = NULL, last_name = NULL, nick_name = NULL;
DROP TABLE IF EXISTS users;

-- Test of ERR_CHECK_CONSTRAINT_VIOLATION error
-- Simple test of UPDATE enforcing CHECK constraints
CREATE TABLE tbl (id integer PRIMARY KEY CHECK (age < 20), age INTEGER);
INSERT INTO tbl VALUES (1, 12);
INSERT INTO tbl VALUES (2, 15);
SELECT * from tbl;
UPDATE tbl SET age = age + 100;
SELECT * from tbl;
DROP TABLE tbl;

-- Test of ERR_CHECK_CONSTRAINT_VIOLATION error
-- Slightly more exhaustive test of UPDATE enforcing CHECK constraints
CREATE TABLE products ( product_no integer PRIMARY KEY CHECK (product_no > 2) CHECK (product_no < 10), CHECK ((name || product_no) < 'Lord8'), name text CHECK (name > 'Cereal') CHECK (name < 'Zero'));
INSERT INTO products VALUES (3, 'Lord');
UPDATE products SET product_no = 1;
UPDATE products SET product_no = 15;
UPDATE products SET product_no = 8;
UPDATE products SET product_no = 7, name = 'Cereal';
UPDATE products SET product_no = 7, name = 'Zero';
SELECT * from products;
DROP TABLE products;

-- Test that UPDATE does not issue ERR_CHECK_CONSTRAINT_VIOLATION error when not expected
CREATE TABLE products ( product_no integer PRIMARY KEY CHECK (product_no > 2) CHECK (product_no < 10), CHECK ((name || product_no) < 'Lord8'), name text CHECK (name > 'Cereal') CHECK (name < 'Zero'));
INSERT INTO products VALUES (3, 'Lord');
SELECT * from products;
UPDATE products SET product_no = 9, name = 'Dummy';
SELECT * from products;
UPDATE products SET product_no = 8, name = 'Lorc';
SELECT * from products;
UPDATE products SET product_no = 7, name = 'Lord';
SELECT * from products;
DROP TABLE products;

-- Test of ERR_CHECK_CONSTRAINT_VIOLATION in UPDATE with a WHERE clause using = (i.e. key-fixing optimization)
-- Also test CHECK constraint that uses a column which is IN the UPDATE SET list of columns
CREATE TABLE products (product_no integer PRIMARY KEY CHECK ((product_no > 2) and (name2 > 'abcd')) CHECK ((product_no > 2) and (name2 > 'abcd')), name1 text, name2 text);
-- CREATE TABLE products (product_no integer PRIMARY KEY, name1 text, name2 text);
INSERT INTO products VALUES (3, 'abcd', 'xyz');
SELECT * from products;
UPDATE products SET product_no = 2, name1 = 'mno' WHERE name2 = 'xyz';
SELECT * from products;
DROP TABLE products;

-- Test of ERR_CHECK_CONSTRAINT_VIOLATION in UPDATE with a WHERE clause using = (i.e. key-fixing optimization)
-- This is similar to the previous test but in this case, "name1" column is not used in SET clause or CHECK constraint
CREATE TABLE products (product_no integer PRIMARY KEY CHECK ((product_no > 2) and (name2 > 'abcd')) CHECK ((product_no > 2) and (name2 > 'abcd')), name1 text, name2 text);
INSERT INTO products VALUES (3, 'abcd', 'xyz');
SELECT * from products;
UPDATE products SET product_no = 2 WHERE name2 = 'xyz';
SELECT * from products;
DROP TABLE products;

-- Test of ERR_CHECK_CONSTRAINT_VIOLATION in UPDATE with a WHERE clause using != (i.e. NO key-fixing optimization)
CREATE TABLE products (product_no integer PRIMARY KEY CHECK ((product_no > 2) and (name2 > 'abcd')) CHECK ((product_no > 2) and (name2 > 'abcd')), name1 text, name2 text);
INSERT INTO products VALUES (3, 'abcd', 'xyz');
SELECT * from products;
UPDATE products SET product_no = 2, name1 = 'mno' WHERE name2 != 'mno';
SELECT * from products;
DROP TABLE IF EXISTS products;

-- Test CHECK constraint that uses a column which is NOT IN the UPDATE SET list of columns
-- In the below case, we don't expect any error.
CREATE TABLE products (product_no integer PRIMARY KEY CHECK (name2 > 'abcd'), name1 text, name2 text);
INSERT INTO products VALUES (3, 'abcd', 'xyz');
SELECT * from products;
UPDATE products SET product_no = 2, name1 = 'mno' WHERE name2 = 'xyz';
SELECT * from products;
DROP TABLE IF EXISTS products;

-- Test of ERR_DROP_FUNCTION_DEPENDS error (simple test)
DROP FUNCTION IF EXISTS SAMEVALUE(INTEGER);
CREATE FUNCTION SAMEVALUE(INTEGER) RETURNS INTEGER AS $$samevalue^functions;
DROP TABLE IF EXISTS products;
CREATE TABLE products (product_no INTEGER, name TEXT, price NUMERIC CONSTRAINT name1 CHECK (price * samevalue(product_no) < 1000));
SELECT '-- Expect ERR_DROP_FUNCTION_DEPENDS error in a DROP FUNCTION AFTER 1 CREATE TABLE and 0 DROP TABLE';
DROP FUNCTION SAMEVALUE(INTEGER);
DROP TABLE products;
SELECT '-- Expect NO ERR_DROP_FUNCTION_DEPENDS error in a DROP FUNCTION AFTER 1 CREATE TABLE and 1 DROP TABLE';
DROP FUNCTION SAMEVALUE(INTEGER);

-- Test of ERR_DROP_FUNCTION_DEPENDS error (fancier test)
DROP FUNCTION IF EXISTS SAMEVALUE(INTEGER);
CREATE FUNCTION SAMEVALUE(INTEGER) RETURNS INTEGER AS $$samevalue^functions;
CREATE TABLE products1 (product_no integer, name text, price numeric CONSTRAINT name1 CHECK (samevalue(product_no) < 1000));
SELECT '-- Expect ERR_DROP_FUNCTION_DEPENDS error (constraint NAME1) in a DROP FUNCTION AFTER 1 CREATE TABLE and 0 DROP TABLE';
DROP FUNCTION SAMEVALUE(INTEGER);
CREATE TABLE products2 (product_no integer, name text, price numeric CONSTRAINT name2 CHECK (samevalue(product_no) < 1000));
SELECT '-- Expect ERR_DROP_FUNCTION_DEPENDS error (constraint NAME1) in a DROP FUNCTION AFTER 2 CREATE TABLE and 0 DROP TABLE';
DROP FUNCTION SAMEVALUE(INTEGER);
DROP TABLE products1;
SELECT '-- Expect ERR_DROP_FUNCTION_DEPENDS error (constraint NAME2) in a DROP FUNCTION AFTER 2 CREATE TABLE and 1 DROP TABLE';
DROP FUNCTION SAMEVALUE(INTEGER);
DROP TABLE products2;
SELECT '-- Expect ERR_DROP_FUNCTION_DEPENDS error (constraint NAME2) in a DROP FUNCTION AFTER 2 CREATE TABLE and 2 DROP TABLE';
DROP FUNCTION SAMEVALUE(INTEGER);

-- Test of ERR_CHECK_CONSTRAINT_VIOLATION in INSERT INTO and UPDATE in a table containing MULTIPLE primary key columns
DROP TABLE IF EXISTS products;
CREATE TABLE products (id0 INTEGER primary key, id1 INTEGER KEY NUM 1, name VARCHAR, CHECK (id1 > 0), CHECK (name < 'mnop'));
INSERT INTO products VALUES (1, 2, 'abcd');
INSERT INTO products VALUES (2, 3, 'efgh');
INSERT INTO products VALUES (2, 0, 'ijkl');
INSERT INTO products VALUES (3, -1, 'ijkl');
INSERT INTO products VALUES (4, 2, 'mnop');
UPDATE products SET id1 = id1 - 2;
UPDATE products SET name = 'mnop' where id0 = 1;

-- Test of ERR_CHECK_CONSTRAINT_VIOLATION with functions and casting
CREATE FUNCTION IF NOT EXISTS length(VARCHAR) RETURNS INTEGER AS $L;
DROP TABLE IF EXISTS clients;
CREATE TABLE clients (
    id            NUMERIC      CONSTRAINT clients PRIMARY KEY,
    zip           CHAR(5)      CONSTRAINT zip_length CHECK (length(zip) = 5)
);
INSERT INTO clients
VALUES
(1, '11223'),
(2, '11225');
SELECT * FROM clients;
UPDATE clients SET zip = '11228' WHERE id = 1;
SELECT * FROM clients;
UPDATE clients SET zip = (zip::numeric - 10000)::varchar;
SELECT * FROM clients;
-- Now try to drop function when it's still being used; should fail
DROP FUNCTION IF EXISTS length(VARCHAR);
-- Now drop table then function
DROP TABLE clients;
DROP FUNCTION length(VARCHAR);

-- Test of ERR_CHECK_CONSTRAINT_VIOLATION with IN
DROP TABLE IF EXISTS product_details;
CREATE TABLE product_details(
    agency_code VARCHAR(10) NOT NULL,
    product_code VARCHAR(40) NOT NULL,
    distribution_channel VARCHAR(15) NOT NULL ,
    claim VARCHAR(5) NOT NULL CHECK(CLAIM IN ('No','Yes'))
);
INSERT INTO product_details(agency_code,product_code,distribution_channel,claim)
VALUES ('AA','BB','CC','Yes');
SELECT * FROM product_details;
UPDATE product_details SET claim = 'No';
SELECT * FROM product_details;
UPDATE product_details SET claim = 'foo';
DROP TABLE product_details;

-- Test of ERR_CHECK_CONSTRAINT_VIOLATION with RegEx using LIKE
DROP TABLE IF EXISTS users;
CREATE TABLE users(
    username VARCHAR(30) CONSTRAINT user_name_pk PRIMARY KEY,
    password VARCHAR(60) CONSTRAINT user_password_nn NOT NULL,
    email VARCHAR(60) CONSTRAINT user_email_nn NOT NULL,
    CONSTRAINT user_email_chk CHECK(email LIKE '%@%.%')
);
INSERT INTO users(username, password, email)
VALUES
('sam','catdog.33','sam@zzz.com'),
('nars','fatdog.44','nars@zzz.com');
SELECT * FROM users;
update users set email = 'sam@yyy.com' where username = 'sam';
select * from users;
update users set email = 'foocoo' where username = 'nars';
DROP TABLE users;

-- Test of ERR_CHECK_CONSTRAINT_VIOLATION with RegEx using SIMILAR TO
CREATE TABLE users(
username VARCHAR(30) CONSTRAINT user_name_pk PRIMARY KEY,
password VARCHAR(60) CONSTRAINT user_password_nn NOT NULL,
email VARCHAR(60) CONSTRAINT user_email_nn NOT NULL,
phone VARCHAR(10) CONSTRAINT phone_check CHECK(phone SIMILAR TO '[[:digit:]]{10}')
);
INSERT into USERS
VALUES
('sam','nnnnnn.33','foo@boo.com','8888888888'),
('bam','nnnnnn.33','bam@boo.com','9999999999');
SELECT * FROM users;
UPDATE users SET phone = '906222' WHERE username = 'sam';
DROP TABLE users;

-- Test of ERR_CHECK_CONSTRAINT_VIOLATION with BETWEEN
DROP TABLE IF EXISTS domains;
CREATE TABLE domains (
  domain_id INTEGER PRIMARY KEY,
  domain VARCHAR(255) UNIQUE NOT NULL,
  uid INT NOT NULL CHECK(uid BETWEEN 1 AND 65535),
  gid INT NOT NULL CHECK(gid BETWEEN 1 AND 65535)
);
INSERT INTO domains(domain_id, domain, uid, gid) VALUES
(1,'foo doo koo',22,22),
(2,'foo doo boo',23,23);
SELECT * FROM domains;
UPDATE domains SET domain = 'foo noo roo' WHERE domain_id = 1;
SELECT * FROM domains;
UPDATE domains SET uid = 25, gid = 25 WHERE uid = 22;
SELECT * FROM domains;
UPDATE domains SET uid = 250000, gid = 25 WHERE uid = 25;
DROP TABLE domains;

-- Test of ERR_CHECK_CONSTRAINT_VIOLATION with OR with casting
-- Test updating a NUMERIC field to NULL works (previously errored with %YDB-E-ZYSQLNULLNOTVALID)
DROP TABLE IF EXISTS country;
CREATE TABLE country (
	    code CHARACTER(3) NOT NULL,
	    name TEXT NOT NULL,
	    continent TEXT NOT NULL,
	    region TEXT NOT NULL,
	    surfacearea NUMERIC NOT NULL,
	    indepyear SMALLINT,
	    population INTEGER NOT NULL,
	    lifeexpectancy NUMERIC,
	    gnp NUMERIC(10,2),
	    gnpold NUMERIC(10,2),
	    localname TEXT NOT NULL,
	    governmentform TEXT NOT NULL,
	    headofstate TEXT,
	    capital INTEGER,
	    code2 CHARACTER(2) NOT NULL,
	    CONSTRAINT country_continent_check CHECK (((((((continent = 'Asia'::text) OR (continent = 'Europe'::text)) OR (continent = 'North America'::text)) OR (continent = 'Africa'::text)) OR (continent = 'Australia'::text)) OR (continent = 'Antarctica'::text)) OR (continent = 'South America'::text))
);
INSERT INTO country VALUES ('FRA', 'France', 'Europe', 'Western Europe', 551500, 843, 59225700, 78.800003, 1424285.00, 1392448.00, 'France', 'Republic', 'Emmanuel Macron', 2974, 'FR');
SELECT * FROM country;
UPDATE country SET continent = 'Europa';
UPDATE country SET gnp = NULL;
DROP TABLE country;

-- Test of ERR_CHECK_CONSTRAINT_VIOLATION with dates
DROP TABLE IF EXISTS Course_Enrollment_Session;
CREATE TABLE Course_Enrollment_Session (
 ID INT PRIMARY KEY,
 Year INT NOT NULL,
 Start_Registration_Date DATE NOT NULL,
 End_Registration_Date DATE CHECK(End_Registration_Date > Start_Registration_Date)
);
INSERT INTO Course_Enrollment_Session
VALUES
(1,2020,date'2020-02-11',date'2020-02-12'),
(2,2021,date'2021-02-11',date'2021-02-12')
;
SELECT * FROM Course_Enrollment_Session;
UPDATE Course_Enrollment_Session SET End_Registration_Date = date'2019-02-12' WHERE ID = 1;
SELECT * FROM Course_Enrollment_Session;
UPDATE Course_Enrollment_Session SET End_Registration_Date = date'2022-02-12' WHERE ID = 2;
SELECT * FROM Course_Enrollment_Session;
UPDATE Course_Enrollment_Session SET End_Registration_Date = NULL WHERE ID = 2;
SELECT * FROM Course_Enrollment_Session;
DROP TABLE Course_Enrollment_Session;

-- Test of ERR_CHECK_CONSTRAINT_VIOLATION with COALESCE function
DROP TABLE IF EXISTS concepts;
CREATE TABLE concepts(
	concept_id INTEGER PRIMARY KEY,
	standard_concept VARCHAR(1),
	CONSTRAINT chk_c_standard_concept CHECK (COALESCE(standard_concept,'C') IN ('C','S'))
);
INSERT INTO concepts
VALUES
(1,'S'),
(2,NULL),
(3,'C'),
(4,NULL);
SELECT * FROM concepts;
UPDATE concepts SET standard_concept = NULL;
SELECT * FROM concepts;
UPDATE concepts SET standard_concept = 'Q';
DROP TABLE concepts;

-- Test of https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1179#note_1057367513
DROP TABLE IF EXISTS country;
CREATE TABLE country (gnp numeric(4,2));
INSERT INTO country VALUES (1.25);
-- The below used to previously issue a `%YDB-E-ZYSQLNULLNOTVALID` error.
INSERT INTO country VALUES (NULL);
-- The below used to previously issue a `%YDB-E-ZYSQLNULLNOTVALID` error.
UPDATE country SET gnp = NULL;
SELECT gnp is NULL from country;

-- Test numeric/string literals where column name is expected in UNIQUE constraint issues syntax error
CREATE TABLE abcd (id1 INTEGER, UNIQUE (3));
CREATE TABLE abcd (id1 INTEGER, UNIQUE ('abcd'));

-- Test that specifying a table level UNIQUE constraint (i.e. a UNIQUE constraint with a list of columns)
-- as a column level UNIQUE constraint issues a syntax error
CREATE TABLE abcd (id1 INTEGER, id2 INTEGER UNIQUE (id1));
CREATE TABLE abcd (id1 INTEGER, id2 INTEGER UNIQUE (id1, id2));

-- Test that specifying a column-level UNIQUE constraint where a table-level UNIQUE constraint is expected issues a syntax error.
CREATE TABLE tmp (id INTEGER, UNIQUE);

-- Test that specifying multiple CHECK constraints in one table level constraint issues a syntax error
CREATE TABLE abcd (id1 INTEGER, CHECK (id1 > 2) CHECK (id1 > 3));

-- Test that specifying multiple UNIQUE constraints in one table level constraint issues a syntax error
CREATE TABLE abcd (id1 INTEGER, UNIQUE (id1) UNIQUE (id1));

-- Test that mixing UNIQUE and CHECK constraints in one table level constraint issues a syntax error
CREATE TABLE abcd (id1 INTEGER, CHECK (id1 > 2) UNIQUE (id1));

-- Test of ERR_UNKNOWN_COLUMN_NAME error in UNIQUE constraint
CREATE TABLE abcd (id1 INTEGER, UNIQUE (id2));
CREATE TABLE abcd (id1 INTEGER, UNIQUE (id1, id2));

-- Test of ERR_DUPLICATE_COLUMN error in UNIQUE constraint
CREATE TABLE abcd (id1 INTEGER, UNIQUE (id1, id1));
CREATE TABLE abcd (id1 INTEGER, id2 INTEGER, UNIQUE (id1, id2, id1));

-- Test that table-level UNIQUE constraint specifying a list of expressions issues a syntax error
CREATE TABLE abcd (id1 INTEGER, id2 INTEGER, UNIQUE (id1 + id2, id1));

-- Test of ERR_DUPLICATE_CONSTRAINT error within multiple UNIQUE constraints
-- Test of explicitly specified constraint name collision between 2 column-level UNIQUE constraints
CREATE TABLE abcd (id INTEGER CONSTRAINT uniq1 UNIQUE, name VARCHAR CONSTRAINT uniq1 UNIQUE);
-- Test of explicitly specified constraint name collision between 2 table-level UNIQUE constraints
CREATE TABLE abcd (id1 INTEGER, id2 INTEGER, CONSTRAINT uniq2 UNIQUE (id1, id2), CONSTRAINT uniq2 UNIQUE (id2, id1));
-- Test of explicitly specified constraint name collision between 1 column-level and 1 table-level UNIQUE constraint
CREATE TABLE abcd (id1 INTEGER, CONSTRAINT uniq3 UNIQUE (id1, id2), id2 integer CONSTRAINT uniq3 UNIQUE);
-- Test explicitly specified UNIQUE constraint name that collides with a previously specified auto generated UNIQUE constraint
CREATE TABLE abcd (id1 INTEGER UNIQUE, id2 INTEGER CONSTRAINT abcd_id1_key UNIQUE);
-- Test name collision among multiple 63-byte user specified column-level constraint names (longest allowed name length)
CREATE TABLE abcd (
	     id1 integer CONSTRAINT toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqrstu UNIQUE,
	     id2 integer CONSTRAINT toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqrstu UNIQUE
	);
-- Test name collision among multiple 63-byte user specified table-level constraint names (longest allowed name length)
CREATE TABLE abcd (
	     id1 integer,
	     id2 integer,
	     CONSTRAINT toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqrstu UNIQUE (id1, id2),
	     CONSTRAINT toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqrstu UNIQUE (id2, id1)
	);
-- Test explicitly specified CHECK constraint name that collides with a previously specified auto generated UNIQUE constraint name
CREATE TABLE abcd (id1 INTEGER UNIQUE, id2 INTEGER CONSTRAINT abcd_id1_key CHECK (id2 > 1));
-- Test explicitly specified UNIQUE constraint name that collides with a previously specified auto generated CHECK constraint name
CREATE TABLE abcd (id1 INTEGER CHECK (id1 > 1), id2 INTEGER CONSTRAINT abcd_id1_check UNIQUE);

-- Test ERR_DUPLICATE_KEY_VALUE error for a UNIQUE constraint on an INSERT INTO of numeric data
CREATE TABLE tmp (id1 INTEGER, id2 INTEGER, UNIQUE(id1, id2));
INSERT INTO tmp VALUES (3, 4);
INSERT INTO tmp VALUES (3, 4);
SELECT * FROM tmp;
DROP TABLE tmp;

-- Test ERR_DUPLICATE_KEY_VALUE error for a UNIQUE constraint on an INSERT INTO of string data
CREATE TABLE tmp (id1 VARCHAR, id2 VARCHAR, UNIQUE(id1, id2));
INSERT INTO tmp VALUES ('abcd efgh', 'ijkl mnop');
INSERT INTO tmp VALUES ('abcd efgh', 'ijkl mnop');
SELECT * FROM tmp;
DROP TABLE tmp;

-- Test NO ERR_DUPLICATE_KEY_VALUE error for a UNIQUE constraint on an INSERT INTO of NULL data
CREATE TABLE tmp (id1 VARCHAR, id2 VARCHAR, UNIQUE(id1, id2));
INSERT INTO tmp VALUES (NULL, NULL);
INSERT INTO tmp VALUES (NULL, NULL);
SELECT * FROM tmp;
DROP TABLE tmp;

-- Fancier test of ERR_DUPLICATE_KEY_VALUE error for a UNIQUE constraint on an INSERT INTO of numeric data
CREATE TABLE TMP (id1 INTEGER, id2 INTEGER, id3 INTEGER, UNIQUE (id1, id2), UNIQUE(id2, id3));
INSERT INTO tmp VALUES (1, 2, 3);
INSERT INTO tmp VALUES (2, 2, 5);
-- The below query should issue a ERR_DUPLICATE_KEY_VALUE error due to UNIQUE(id2, id3) constraint violation
INSERT INTO tmp VALUES (3, 2, 3);
-- The below query would have normally issued a ERR_DUPLICATE_KEY_VALUE error due to UNIQUE(id1, id2) constraint violation
-- but since the previous row did not get inserted, this should not issue any error. Test that.
INSERT INTO tmp VALUES (3, 2, 4);
SELECT * FROM tmp;
DROP TABLE tmp;

-- Test that DELETE works with INSERT INTO to maintain/enforce the UNIQUE constraint
CREATE TABLE tmp (id1 INTEGER, id2 INTEGER, id3 INTEGER, UNIQUE(id1, id3));
INSERT INTO tmp VALUES (1,2,3);
INSERT INTO tmp VALUES (2,3,3);
INSERT INTO tmp VALUES (NULL,3,3);
INSERT INTO tmp VALUES (NULL,3,NULL);
SELECT * FROM tmp;
-- The below query should issue a ERR_DUPLICATE_KEY_VALUE error due to UNIQUE(id1, id3) constraint violation
INSERT INTO tmp VALUES (2,4,3);
SELECT * FROM tmp;
-- The below DELETE should delete the three rows (2,3,3), (NULL,3,3), (NULL,3,NULL)
DELETE FROM tmp WHERE id2 = 3;
SELECT * FROM tmp;
-- The below query should no longer issue a ERR_DUPLICATE_KEY_VALUE error now that (2,3,3) has been deleted
-- This tests that the above DELETE must have done something with the UNIQUE constraint maintenance to enable
-- a row with an id2 value of 4 to henceforth be allowed.
-- Additionally, this also tests that DELETE works fine with deleting NULL values of columns that are part of
-- UNIQUE constraints (since the deleted rows above include NULL values for the id1 and id3 columns).
INSERT INTO tmp VALUES (2,4,3);
SELECT * FROM tmp;
DROP TABLE tmp;

---------------------------------------------------------------------------
-- Test that UPDATE works fine when UNIQUE constraint is not violated
---------------------------------------------------------------------------
-- Test where one column is updated based on other column value
-- The rows (3,5), (4,4), (5,3) will become (6,5), (5,4), (4,3) and so there are no constraint violations.
CREATE TABLE tmp (id1 INTEGER, id2 INTEGER, UNIQUE(id1, id2));
INSERT INTO tmp VALUES (3, 5);
INSERT INTO tmp VALUES (4, 4);
INSERT INTO tmp VALUES (5, 3);
SELECT * FROM tmp;
UPDATE TMP SET id1 = id2 + 1;
SELECT * FROM tmp;
DROP TABLE tmp;

-- Test to swap column values
-- The rows (3,4), (5,3) will become (4,3), (3,5) and so there are no constraint violations.
CREATE TABLE tmp (id1 INTEGER, id2 INTEGER, UNIQUE(id1, id2));
INSERT INTO tmp VALUES (3, 4);
INSERT INTO tmp VALUES (5, 3);
SELECT * FROM tmp;
UPDATE TMP SET id1 = id2, id2 = id1;
SELECT * FROM tmp;
DROP TABLE tmp;

-- Test that NULL does not cause UNIQUE constraint violations
CREATE TABLE tmp (id1 INTEGER, id2 INTEGER, UNIQUE(id2));
INSERT INTO tmp VALUES (3, 5);
INSERT INTO tmp VALUES (4, 4);
INSERT INTO tmp VALUES (5, 3);
SELECT * FROM tmp;
-- Test non-NULL to NULL transition
UPDATE TMP SET id2 = NULL;
SELECT * FROM tmp;
-- Test that we are able to re-insert the original 3 rows after the NULL update without violating any UNIQUE constraints
INSERT INTO tmp VALUES (3, 5);
INSERT INTO tmp VALUES (4, 4);
INSERT INTO tmp VALUES (5, 3);
SELECT * FROM tmp;
-- Test NULL to non-NULL transition
UPDATE TMP SET id2 = id1 - 3 where id2 is NULL;
SELECT * FROM tmp;
DROP TABLE tmp;

---------------------------------------------------------------------------
-- Test of ERR_DUPLICATE_KEY_VALUE error from UPDATE
---------------------------------------------------------------------------
-- Test where both columns are updated based on other column value
-- The rows (3,5), (4,4), (5,3) will become (6,2), (5,3), (4,4)
-- Even though the new 3 rows are unique amongst themselves, the new second row (5,3) conflicts
-- with the pre-existing 3rd row (5,3) and so we expect an error (this matches Postgres behavior).
CREATE TABLE tmp (id1 INTEGER, id2 INTEGER, UNIQUE(id1, id2));
INSERT INTO tmp VALUES (3, 5);
INSERT INTO tmp VALUES (4, 4);
INSERT INTO tmp VALUES (5, 3);
SELECT * FROM tmp;
UPDATE TMP SET id1 = id2 + 1, id2 = id1 - 1;
SELECT * FROM tmp;
DROP TABLE tmp;

-- The rows (3,2), (3,5) will become (3,4), (3,4) resulting in a UNIQUE constraint violation
-- So we expect an error.
CREATE TABLE tmp (id1 INTEGER, id2 INTEGER, UNIQUE(id1, id2));
INSERT INTO tmp VALUES (3, 2);
INSERT INTO tmp VALUES (3, 5);
SELECT * FROM tmp;
UPDATE TMP SET id2 = 4;
SELECT * FROM tmp;
DROP TABLE tmp;

---------------------------------------------------------------------------
-- Test UPDATE when UNIQUE and CHECK constraints are used
---------------------------------------------------------------------------
CREATE TABLE tmp (id1 INTEGER, id2 INTEGER, id3 INTEGER, UNIQUE(id1, id2), UNIQUE(id2, id3), CHECK (id1 > 2));
-- Expect no error for below command
INSERT INTO tmp VALUES (3, 4, 2);
INSERT INTO tmp VALUES (4, 3, 4);
INSERT INTO tmp VALUES (5, 4, 5);
-- Expect a ERR_CHECK_CONSTRAINT_VIOLATION error for below command
INSERT INTO tmp VALUES (1, 2, 3);
-- Expect a ERR_DUPLICATE_KEY_VALUE error for UNIQUE(id1, id2) constraint for below command
UPDATE tmp SET id1 = 4 WHERE id2 > 2;
-- Expect a ERR_DUPLICATE_KEY_VALUE error for UNIQUE(id2, id3) constraint for below command
UPDATE tmp SET id3 = id3 - id2, id2 = 5 WHERE id2 > 2;
SELECT * FROM tmp;
DROP TABLE tmp;

-- Test of ERR_TABLE_KEY_NUM error
-- Test that table level PRIMARY KEY 1-column constraint and KEY NUM usage issues ERR_TABLE_KEY_NUM error
create table products (id integer key num 0, primary key (id));
create table products (id integer key num 0, constraint prikey primary key (id));
-- Test that table level PRIMARY KEY 2-column constraint and KEY NUM usage issues ERR_TABLE_KEY_NUM error
create table products (id1 integer, id2 integer key num 1, constraint prikey primary key (id1, id2));
-- Test that table level PRIMARY KEY 2-column constraint and KEY NUM usage issues ERR_TABLE_KEY_NUM error
-- even though the KEY NUM is in sync with the PRIMARY KEY usage (i.e. id1 is KEY NUM 0 and id2 is KEY NUM 1
-- just like the order the columns are defined in the PRIMARY KEY constraint).
create table products (id1 integer key num 0, id2 integer key num 1, constraint prikey primary key (id1, id2));
-- Test that table level PRIMARY KEY 2-column constraint and duplicate KEY NUM 1 usage issues ERR_TABLE_KEY_NUM error
create table products (id1 integer key num 1, id2 integer key num 1, primary key (id1, id2));
-- One more test of ERR_TABLE_KEY_NUM error. This used to assert fail before in an interim version of the code.
create table tmp (id1 integer KEY NUM 2, id2 integer, id3 integer, PRIMARY KEY (id2, id1, id3));
-- Test that table level PRIMARY KEY constraint does not accept KEY NUM keywords (issues a syntax error)
create table products (id integer, constraint prikey primary key (id) key num 0);
-- Test that column level PRIMARY KEY constraint accepts KEY NUM usage without error as long as it is valid
create table tmp (id integer primary key key num 0);
drop table tmp;
create table tmp (id integer key num 0 primary key);
drop table tmp;
-- Test that column level PRIMARY KEY constraint does not accept KEY NUM usage without error when latter number is invalid
-- Test of ERR_MISSING_KEY error
create table tmp (id integer primary key key num 1);

---------------------------------------------------------------------------
-- Below tests are very similar to those done for UNIQUE constraints in this same file in a previous section
---------------------------------------------------------------------------
-- Test numeric/string literals where column name is expected in PRIMARY KEY constraint issues syntax error
CREATE TABLE abcd (id1 INTEGER, PRIMARY KEY (3));
CREATE TABLE abcd (id1 INTEGER, PRIMARY KEY ('abcd'));

-- Test that specifying a table level PRIMARY KEY constraint (i.e. a PRIMARY KEY constraint with a list of columns)
-- as a column level PRIMARY KEY constraint issues a syntax error
CREATE TABLE abcd (id1 INTEGER, id2 INTEGER PRIMARY KEY (id1));
CREATE TABLE abcd (id1 INTEGER, id2 INTEGER PRIMARY KEY (id1, id2));

-- Test that specifying a column-level PRIMARY KEY constraint where a table-level PRIMARY KEY constraint is expected issues a syntax error.
CREATE TABLE tmp (id INTEGER, PRIMARY KEY);

-- Test that specifying multiple PRIMARY KEY constraints in one table level constraint issues a syntax error
CREATE TABLE abcd (id1 INTEGER, PRIMARY KEY (id1) PRIMARY KEY (id1));

-- Test that mixing PRIMARY KEY and CHECK constraints in one table level constraint issues a syntax error
CREATE TABLE abcd (id1 INTEGER, CHECK (id1 > 2) PRIMARY KEY (id1));

-- Test that mixing PRIMARY KEY and UNIQUE constraints in one table level constraint issues a syntax error
CREATE TABLE abcd (id1 INTEGER, UNIQUE (id1) PRIMARY KEY (id1));

-- Test that mixing PRIMARY KEY, UNIQUE and CHECK constraints in one table level constraint issues a syntax error
CREATE TABLE abcd (id1 INTEGER, CHECK (id1 > 2) UNIQUE (id1) PRIMARY KEY (id1));

-- Test of ERR_UNKNOWN_COLUMN_NAME error in PRIMARY KEY constraint
CREATE TABLE abcd (id1 INTEGER, PRIMARY KEY (id2));
CREATE TABLE abcd (id1 INTEGER, PRIMARY KEY (id1, id2));

-- Test of ERR_DUPLICATE_COLUMN error in PRIMARY KEY constraint
CREATE TABLE abcd (id1 INTEGER, PRIMARY KEY (id1, id1));
CREATE TABLE abcd (id1 INTEGER, id2 INTEGER, PRIMARY KEY (id1, id2, id1));

-- Test that table-level PRIMARY KEY constraint specifying a list of expressions issues a syntax error
CREATE TABLE abcd (id1 INTEGER, id2 INTEGER, PRIMARY KEY (id1 + id2, id1));

-- Test explicitly specified CHECK constraint name that collides with a previously specified auto generated PRIMARY KEY constraint name
CREATE TABLE abcd (id1 INTEGER PRIMARY KEY, id2 INTEGER CONSTRAINT abcd_id1_key CHECK (id2 > 1));

-- Test explicitly specified PRIMARY KEY constraint name that collides with a previously specified auto generated CHECK constraint name
CREATE TABLE abcd (id1 INTEGER CHECK (id1 > 1), id2 INTEGER CONSTRAINT abcd_id1_check PRIMARY KEY);

-- Test explicitly specified UNIQUE constraint name that collides with a previously specified auto generated PRIMARY KEY constraint name
CREATE TABLE abcd (id1 INTEGER PRIMARY KEY, id2 INTEGER CONSTRAINT abcd_id1_key UNIQUE);

-- Test explicitly specified PRIMARY KEY constraint name that collides with a previously specified auto generated UNIQUE constraint name
CREATE TABLE abcd (id1 INTEGER UNIQUE, id2 INTEGER CONSTRAINT abcd_id1_key PRIMARY KEY);

-- Test ERR_DUPLICATE_KEY_VALUE error
-- Test case from https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/582#note_1054688708
DROP TABLE IF EXISTS employees;
CREATE TABLE employees(ID INTEGER CONSTRAINT PK_ID PRIMARY KEY, AGE INTEGER);
INSERT INTO employees(ID,AGE) VALUES (1,22), (2,25);
UPDATE employees set id = 2 where age = 22;
DROP TABLE employees;

-- Test ERR_DUPLICATE_KEY_VALUE error
-- Test case from https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/770#note_1072505608
DROP TABLE IF EXISTS employees;
CREATE TABLE employees(ID INTEGER PRIMARY KEY, AGE INTEGER);
INSERT INTO employees(ID,AGE) VALUES (1,22), (2,25);
UPDATE employees set id = 2 where age = 22;
SELECT * FROM employees;
DROP TABLE employees;

-- Test ERR_DUPLICATE_KEY_VALUE error for a PRIMARY KEY constraint on an INSERT INTO of numeric data
CREATE TABLE tmp (id1 INTEGER, id2 INTEGER, PRIMARY KEY (id1, id2));
INSERT INTO tmp VALUES (3, 4);
INSERT INTO tmp VALUES (3, 4);
SELECT * FROM tmp;
DROP TABLE tmp;

-- Test ERR_DUPLICATE_KEY_VALUE error for a PRIMARY KEY constraint on an INSERT INTO of string data
CREATE TABLE tmp (id1 VARCHAR, id2 VARCHAR, PRIMARY KEY(id1, id2));
INSERT INTO tmp VALUES ('abcd efgh', 'ijkl mnop');
INSERT INTO tmp VALUES ('abcd efgh', 'ijkl mnop');
SELECT * FROM tmp;
DROP TABLE tmp;

-- Test ERR_NULL_COL_VALUE error for a PRIMARY KEY constraint on an INSERT INTO of NULL data
CREATE TABLE tmp (id1 VARCHAR, id2 VARCHAR, PRIMARY KEY(id1, id2));
INSERT INTO tmp VALUES (NULL, 'efgh');
INSERT INTO tmp VALUES ('abcd', NULL);
INSERT INTO tmp VALUES (NULL, NULL);
INSERT INTO tmp VALUES ('abcd', 'efgh');
SELECT * FROM tmp;
DROP TABLE tmp;

-- Fancier test of ERR_DUPLICATE_KEY_VALUE error for a PRIMARY KEY constraint on an INSERT INTO of numeric data
CREATE TABLE TMP (id1 INTEGER, id2 INTEGER, id3 INTEGER, PRIMARY KEY (id2, id3));
INSERT INTO tmp VALUES (1, 2, 3);
INSERT INTO tmp VALUES (2, 2, 5);
-- The below query should issue a ERR_DUPLICATE_KEY_VALUE error due to PRIMARY KEY(id2, id3) constraint violation
INSERT INTO tmp VALUES (3, 2, 3);
SELECT * FROM tmp;
DROP TABLE tmp;

-- Test that DELETE works with INSERT INTO to maintain/enforce the PRIMARY KEY constraint
CREATE TABLE tmp (id1 INTEGER, id2 INTEGER, id3 INTEGER, PRIMARY KEY(id1, id3));
INSERT INTO tmp VALUES (1,2,3);
INSERT INTO tmp VALUES (2,3,3);
SELECT * FROM tmp;
-- The below query should issue a ERR_DUPLICATE_KEY_VALUE error due to PRIMARY KEY(id1, id3) constraint violation
INSERT INTO tmp VALUES (2,4,3);
SELECT * FROM tmp;
-- The below DELETE should delete the row (2,3,3)
DELETE FROM tmp WHERE id2 = 3;
SELECT * FROM tmp;
-- The below query should no longer issue a ERR_DUPLICATE_KEY_VALUE error now that (2,3,3) has been deleted
-- This tests that the above DELETE must have done something with the PRIMARY KEY constraint maintenance to enable
-- a row with an id2 value of 4 to henceforth be allowed.
INSERT INTO tmp VALUES (2,4,3);
SELECT * FROM tmp;
DROP TABLE tmp;

---------------------------------------------------------------------------
-- Test that UPDATE works fine when PRIMARY KEY constraint is not violated
---------------------------------------------------------------------------
-- Test where one column is updated based on other column value
-- The rows (3,5), (4,4), (5,3) will become (6,5), (5,4), (4,3) and so there are no constraint violations.
CREATE TABLE tmp (id1 INTEGER, id2 INTEGER, PRIMARY KEY(id1, id2));
INSERT INTO tmp VALUES (3, 5);
INSERT INTO tmp VALUES (4, 4);
INSERT INTO tmp VALUES (5, 3);
SELECT * FROM tmp;
UPDATE TMP SET id1 = id2 + 1;
SELECT * FROM tmp;
DROP TABLE tmp;

-- Test to swap column values
-- The rows (3,4), (5,3) will become (4,3), (3,5) and so there are no constraint violations.
CREATE TABLE tmp (id1 INTEGER, id2 INTEGER, PRIMARY KEY(id1, id2));
INSERT INTO tmp VALUES (3, 4);
INSERT INTO tmp VALUES (5, 3);
SELECT * FROM tmp;
UPDATE TMP SET id1 = id2, id2 = id1;
SELECT * FROM tmp;
DROP TABLE tmp;

-- Test that NULL does cause PRIMARY KEY constraint violations
CREATE TABLE tmp (id1 INTEGER, id2 INTEGER, PRIMARY KEY(id2));
INSERT INTO tmp VALUES (3, 5);
INSERT INTO tmp VALUES (4, 4);
INSERT INTO tmp VALUES (5, 3);
SELECT * FROM tmp;
-- Test non-NULL to NULL transition. Should issue ERR_NULL_COL_VALUE error
UPDATE TMP SET id2 = NULL;
SELECT * FROM tmp;
DROP TABLE tmp;

---------------------------------------------------------------------------
-- Test of ERR_DUPLICATE_KEY_VALUE error from UPDATE
---------------------------------------------------------------------------
-- Test where both columns are updated based on other column value
-- The rows (3,5), (4,4), (5,3) will become (6,2), (5,3), (4,4)
-- Even though the new 3 rows are unique amongst themselves, the new second row (5,3) conflicts
-- with the pre-existing 3rd row (5,3) and so we expect an error (this matches Postgres behavior).
CREATE TABLE tmp (id1 INTEGER, id2 INTEGER, PRIMARY KEY(id1, id2));
INSERT INTO tmp VALUES (3, 5);
INSERT INTO tmp VALUES (4, 4);
INSERT INTO tmp VALUES (5, 3);
SELECT * FROM tmp;
UPDATE TMP SET id1 = id2 + 1, id2 = id1 - 1;
SELECT * FROM tmp;
DROP TABLE tmp;

-- The rows (3,2), (3,5) will become (3,4), (3,4) resulting in a PRIMARY KEY constraint violation
-- So we expect an error.
CREATE TABLE tmp (id1 INTEGER, id2 INTEGER, PRIMARY KEY(id1, id2));
INSERT INTO tmp VALUES (3, 2);
INSERT INTO tmp VALUES (3, 5);
SELECT * FROM tmp;
UPDATE TMP SET id2 = 4;
SELECT * FROM tmp;
DROP TABLE tmp;

---------------------------------------------------------------------------
-- Test UPDATE when PRIMARY KEY, UNIQUE and CHECK constraints are used
---------------------------------------------------------------------------
CREATE TABLE tmp (id1 INTEGER, id2 INTEGER, id3 INTEGER, PRIMARY KEY(id1, id2), UNIQUE(id2, id3), CHECK (id1 > 2));
-- Expect no error for below command
INSERT INTO tmp VALUES (3, 4, 2);
INSERT INTO tmp VALUES (4, 3, 4);
INSERT INTO tmp VALUES (5, 4, 5);
-- Test ERR_CHECK_CONSTRAINT_VIOLATION error from INSERT INTO
INSERT INTO tmp VALUES (1, 2, 3);
-- Test ERR_CHECK_CONSTRAINT_VIOLATION error from UPDATE
UPDATE tmp SET id1 = 1 WHERE id2 > 2;
-- Test ERR_DUPLICATE_KEY_VALUE error from INSERT INTO for PRIMARY KEY constraint
INSERT INTO tmp VALUES (5, 4, 6);
-- Test ERR_DUPLICATE_KEY_VALUE error from INSERT INTO for UNIQUE constraint
INSERT INTO tmp VALUES (6, 3, 4);
-- Test ERR_DUPLICATE_KEY_VALUE error from UPDATE for PRIMARY KEY constraint
UPDATE tmp SET id1 = 4 WHERE id2 > 2;
-- Test ERR_DUPLICATE_KEY_VALUE error from UPDATE for UNIQUE constraint
UPDATE tmp SET id3 = id3 - id2, id2 = 5 WHERE id2 > 2;
SELECT * FROM tmp;
DROP TABLE tmp;

-- Test that PRIMARY KEY constraint name in one table can be the same as a CHECK or UNIQUE constraint in another table.
-- It is only PRIMARY KEY constraint names across all tables that need to be unique.
-- Note that Postgres does not allow PRIMARY KEY constraint name in one table to be the same as a UNIQUE constraint name in
-- another table. It does allow the PRIMARY KEY constraint name in one table to be the same as a CHECK constraint name in
-- another table. Most likely because the underlying B-tree storage for the PRIMARY KEY and UNIQUE constraints in Postgres
-- is tied to the constraint name and therefore needs to be unique to distinguish the two. Octo differs from Postgres
-- in this regard. Since the UNIQUE constraint global named is derived from the table name and the list of column names
-- forming the constraint, Octo does not need this restriction.
CREATE TABLE tmp1 (id1 INTEGER CONSTRAINT tmp_primary_key PRIMARY KEY, id2 INTEGER CONSTRAINT tmp_unique UNIQUE, id3 INTEGER CONSTRAINT tmp_check CHECK (id3 > 1));
-- Test that creating a PRIMARY KEY constraint name in one table that matches the UNIQUE constraint name in another table
-- does not issue any error.
CREATE TABLE tmp2 (id1 INTEGER CONSTRAINT tmp_unique PRIMARY KEY);
-- Test that creating a PRIMARY KEY constraint name in one table that matches the CHECK constraint name in another table
-- does not issue any error.
CREATE TABLE tmp3 (id1 INTEGER CONSTRAINT tmp_check PRIMARY KEY);
-- Test that creating a PRIMARY KEY constraint name in one table that matches the PRIMARY KEY constraint name in another table
-- issues a ERR_DUPLICATE_PRIMARY_KEY_CONSTRAINT error.
CREATE TABLE tmp4 (id1 INTEGER CONSTRAINT tmp_primary_key PRIMARY KEY);
-- Test that creating a CHECK and UNIQUE constraint name in one table that matches the CHECK and UNIQUE constraint names
-- in another table does not issue any error.
CREATE TABLE tmp5 (id1 INTEGER PRIMARY KEY, id2 INTEGER CONSTRAINT tmp_unique UNIQUE, id3 INTEGER CONSTRAINT tmp_check CHECK (id3 > 1));
\d tmp1;
\d tmp2;
\d tmp3;
\d tmp5;
DROP TABLE tmp1;
DROP TABLE tmp2;
DROP TABLE tmp3;
DROP TABLE tmp5;

-- Issue syntax error for qualified column names in CONSTRAINTs, i.e. for `x.y` syntax
create table tmp (id integer, unique (tmp.id));

-- Confirm user-friendly error message for ERR_TYPE_NOT_COMPATIBLE issued for CHECK constraints
create table tmp (id integer CHECK ((id+'abcd')::varchar > 1::varchar));

-- Test of ERR_CHECK_CONSTRAINT_VIOLATION error with CHECK constraint that is a constant (i.e. does not use any table columns)
CREATE TABLE tmp (product_no integer, CHECK (false));
INSERT INTO tmp VALUES (1);
DROP TABLE tmp;

