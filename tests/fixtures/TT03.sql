#################################################################
#								#
# Copyright (c) 2022-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TT03 : OCTO609 : TRUNCATE works with TABLE keyword added

-- Single table case
-- All rows present in `names` table
select * from names;
truncate table names;
-- No rows present in `names` table, but table still exists
select * from names;

-- Multiple table case
drop table if exists customers_cpy;
drop table if exists orders_cpy;
CREATE TABLE customers_cpy(customer_id INTEGER PRIMARY KEY, first_name VARCHAR(8), last_name VARCHAR(10), email VARCHAR(20), address VARCHAR(26), city VARCHAR(16), state VARCHAR(2), zipcode VARCHAR(5));
CREATE TABLE orders_cpy (order_id INTEGER PRIMARY KEY, order_date DATE, order_amount VARCHAR(7), customer_id INTEGER);
insert into customers_cpy (select * from customers);
insert into orders_cpy (select * from orders);
-- All rows present in `customers_cpy` and `orders_cpy` tables
select * from customers_cpy;
select * from orders_cpy;
truncate table customers_cpy, orders_cpy;
-- No rows present in `customers` or `orders` tables, but tables still exist
select * from customers_cpy;
select * from orders_cpy;

-- Non-existent tables case
truncate table badtable, notgoodtable, missingtable;
