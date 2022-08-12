#################################################################
#								#
# Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TCTC003 : OCTO772 : Test various errors in CONSTRAINTS

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
-- Note: The below produces a syntax error currently
-- but will produce a ERR_TABLE_MULTIPLE_PRIMARY_KEYS error once YDBOcto#770 is implemented.
create table products (id integer constraint prikey primary key, name varchar, primary key (id, name));

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
create table products (id integer, firstname varchar CHECK (firstname = 'Zero' || +NULL));
create table products (id integer, firstname varchar CHECK (firstname = 'Zero' || -NULL));

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

-- Test of ERR_IDENT_LENGTH error on constraint name by specifying a 64-byte name (max allowed is 63 bytes)
CREATE TABLE products (
	product_no integer CONSTRAINT toolong1abcdefghijklmnopqrstuvwxyztoolong2abcdefghijklmnopqrstuv CHECK (product_no < 2));

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
(1,2020,'2020-02-11','2020-02-12'),
(2,2021,'2021-02-11','2021-02-12')
;
SELECT * FROM Course_Enrollment_Session;
UPDATE Course_Enrollment_Session SET End_Registration_Date = '2019-02-12' WHERE ID = 1;
SELECT * FROM Course_Enrollment_Session;
UPDATE Course_Enrollment_Session SET End_Registration_Date = '2022-02-12' WHERE ID = 2;
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

