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
SELECT * from products;
DROP TABLE products;

-- Test of ERR_CHECK_CONSTRAINT_VIOLATION error
-- Test of multiple CHECK column constraints on non-key column
CREATE TABLE products (product_no integer, name text, price numeric CHECK (price > 0) CHECK (price > 5));
INSERT INTO products VALUES (1, 'abcd', 6);
INSERT INTO products VALUES (2, 'efgh', 5);
SELECT * from products;
DROP TABLE products;

-- Test of ERR_CHECK_CONSTRAINT_VIOLATION error
-- Test of 1 CHECK column constraint on primary key column
CREATE TABLE products (product_no integer PRIMARY KEY CHECK (product_no > 2), name text);
INSERT INTO products VALUES (3, 'abcd');
INSERT INTO products VALUES (2, 'efgh');
SELECT * from products;
DROP TABLE products;

-- Test of ERR_CHECK_CONSTRAINT_VIOLATION error
-- Test of multiple CHECK column constraints on primary key column
CREATE TABLE products (product_no integer PRIMARY KEY CHECK (product_no > 2) CHECK (product_no < 10), name text);
INSERT INTO products VALUES (5, 'abcd');
INSERT INTO products VALUES (10, 'efgh');
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
SELECT * from products;
DROP TABLE products;

-- Test that check constraint is satisfied if it evalutes to NULL (not just TRUE)
CREATE TABLE products (product_no integer, name text CHECK (product_no < 5));
INSERT INTO products VALUES (4, 'abcd');
INSERT INTO products VALUES (5, 'efgh');
INSERT INTO products VALUES (NULL, 'ijkl');
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
DROP TABLE IF EXISTS users;

