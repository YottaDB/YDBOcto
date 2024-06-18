#################################################################
#								#
# Copyright (c) 2023-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

drop table if exists orders keepdata;
create table orders (order_id INTEGER PRIMARY KEY, order_date DATE, order_amount VARCHAR(7), customer_id INTEGER) GLOBAL "^orders" READONLY;
SELECT o1.order_id, o2.order_id, o1.order_date FROM orders o1 INNER JOIN orders o2 ON (o1.order_id < o2.order_id) order by o1.order_id, o2.order_id;
SELECT o1.order_id, o2.order_id, o1.order_date FROM orders o1 INNER JOIN orders o2 ON (o1.order_id < o2.order_id) WHERE o1.order_date <= DATE '1776-07-04' order by o1.order_id, o2.order_id;
SELECT o1.order_id, o2.order_id, o1.order_date FROM orders o1 INNER JOIN orders o2 ON (o1.order_id < o2.order_id) WHERE o1.order_date >= DATE '1795-07-21' order by o1.order_id, o2.order_id;

SELECT alias1.order_date AS alias2, customers.city AS alias3 FROM customers  RIGHT JOIN orders AS alias1 ON (((customers.customer_id IS NULL)) AND NOT ((customers.first_name <= alias1.order_amount) AND (customers.first_name > '$124.00'))) WHERE (('Charlottesville' != customers.city) OR ((customers.address IS NULL))) ORDER BY alias1.order_date, customers.city;
create view tmpview AS SELECT alias1.order_date AS alias2, customers.city AS alias3 FROM customers  RIGHT JOIN orders AS alias1 ON (((customers.customer_id IS NULL)) AND NOT ((customers.first_name <= alias1.order_amount) AND (customers.first_name > '$124.00'))) WHERE (('Charlottesville' != customers.city) OR ((customers.address IS NULL))) ORDER BY alias1.order_date, customers.city;
select * from tmpview;

-- Following failed because of wrongly placed closing parenthesis
select o1.order_id from orders o1 where (o1.order_date <= date'1784-05-23') AND (o1.order_id > 1) AND (o1.customer_id < 9) OR NOT EXISTS (SELECT o1.order_date) OR (o1.customer_id IS NOT NULL) OR (o1.order_amount IS NULL) OR (o1.customer_id IS NOT NULL) OR (o1.order_id = 1) order by o1.order_id;
