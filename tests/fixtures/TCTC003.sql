#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
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
create table products (id integer, name text check (COUNT(*) is NULL));
create table products (id integer, name text check (COUNT(name) is NULL));
create table products (id integer, name text check (MIN(name) is NULL));
create table products (id integer, name text check (MAX(name) is NULL));
create table products (id integer, name text check (AVG(name) is NULL));
create table products (id integer, name text check (SUM(name) is NULL));

-- Test of ERR_SUBQUERY_CHECK error
create table products (product_no integer, name text, price numeric CONSTRAINT name1 CHECK (price < (select 1000)));
create table products (product_no integer, name text, price numeric CONSTRAINT name1 CHECK (price < (select * from names)));

-- Test of ERR_AGGREGATE_FUNCTION_CHECK and ERR_SUBQUERY_CHECK errors
create table products (id integer, name text check (COUNT(*) = (SELECT 1000)));

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
create table products (id integer, name varchar check (2 > 0) check (x.name is not null));

-- Test of ERR_UNKNOWN_COLUMN_NAME error in CHECK constraint
create table products (product_no integer, name text, price numeric CONSTRAINT name1 CHECK (price < invalid));
create table products (product_no integer, name text, price numeric CONSTRAINT name1 CHECK (price < product));
---- Test error inside function call parameters
create table products (product_no integer, name text, price numeric CONSTRAINT name1 CHECK (price < samevalue(product)));
create table products (product_no integer, name text, price numeric CONSTRAINT name1 CHECK (ABS(id)));

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

