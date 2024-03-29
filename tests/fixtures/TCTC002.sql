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

-- TCTC002 : OCTO772 : Test column level and table level CHECK constraints are accepted

-- Test table level CHECK constraint without a name is accepted
drop table if exists products;
create table products (id integer, check (id > 0));
select * from products;

-- Test table level CHECK constraint with a name is accepted
drop table if exists products;
create table products (id integer, CONSTRAINT name1 check (id > 0));
select * from products;

-- Test column level CHECK constraint without a name is accepted
drop table if exists products;
CREATE TABLE products (product_no integer, name text, price numeric CHECK (price > 0));
select * from products;

-- Test column level CHECK constraint with a name is accepted
drop table if exists products;
CREATE TABLE products (product_no integer, name text, price numeric CONSTRAINT name1 CHECK (price > 0));
select * from products;

