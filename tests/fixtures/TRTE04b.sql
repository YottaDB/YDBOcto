#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- queries found by query generator which failed with original implementation
SELECT ALL orders.order_date FROM orders  RIGHT JOIN orders AS alias1 ON ((orders.customer_id = alias1.order_id) OR (orders.order_id <= ANY (SELECT DISTINCT alias1.customer_id FROM orders alias1 WHERE alias1.customer_id BETWEEN 3 AND 1 ORDER BY alias1.customer_id LIMIT 1)) AND ((orders.order_date <= alias1.order_date) OR NOT (orders.customer_id = alias1.order_id))) WHERE orders.order_amount LIKE '%' ORDER BY orders.order_date;

SELECT DISTINCT alias1.city FROM customers  RIGHT OUTER JOIN customers AS alias1 ON ((customers.customer_id >= 4) AND NOT ((customers.address > alias1.first_name) OR (2 != 4)) AND ((customers.state != alias1.zipcode)) AND NOT (customers.zipcode <= ALL (SELECT ALL alias1.order_amount FROM orders alias1 ORDER BY alias1.order_amount LIMIT 1)) AND (customers.state < ALL (SELECT alias2.order_date FROM orders alias2 ORDER BY alias2.order_date LIMIT 1)) AND (customers.customer_id >= 4) OR NOT (customers.zipcode != alias1.state) OR (customers.address >= alias1.address) AND ((2 != 4)) AND (customers.address = SOME (SELECT ALL alias3.city FROM customers alias3 WHERE alias3.customer_id = ALL (SELECT alias4.customer_id FROM orders alias4 WHERE alias4.order_date = ALL (SELECT DISTINCT alias5.email FROM customers alias5 WHERE alias5.customer_id >= SOME (SELECT DISTINCT alias6.customer_id FROM orders alias6 ORDER BY alias6.customer_id LIMIT 1) ORDER BY alias5.email LIMIT 1) ORDER BY alias4.customer_id LIMIT 1) ORDER BY alias3.city LIMIT 1)) OR NOT (2 != 4)) WHERE customers.first_name LIKE '%' ORDER BY alias1.city;

SELECT DISTINCT (SELECT DISTINCT alias6.first_name FROM customers alias6 ORDER BY alias6.first_name LIMIT 1) AS alias6 FROM orders  INNER JOIN customers AS alias1 ON ((orders.order_date < alias1.state) AND NOT ((4 = 1))) RIGHT OUTER JOIN customers AS alias2 ON (((alias1.zipcode > SOME (SELECT DISTINCT alias2.order_amount FROM orders alias2 ORDER BY alias2.order_amount LIMIT 1))) AND (alias1.customer_id <= ANY (SELECT alias3.customer_id FROM orders alias3 ORDER BY alias3.customer_id LIMIT 1))) RIGHT OUTER JOIN customers AS alias5 ON (((orders.order_date > alias5.state) AND (orders.order_id <= 3))) WHERE orders.order_amount LIKE '%';
