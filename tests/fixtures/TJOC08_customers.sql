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

-- TJOC08 : OCTO800 : %YDB-E-LVUNDEF error when WHERE clause references FROM/JOIN columns in sub-query

-- Below are queries from various comments at https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/800
-- In addition, some queries were added here to test more combinations (e.g. RIGHT JOIN instead of LEFT JOIN etc.)

SELECT 1 FROM customers c1 LEFT JOIN (SELECT order_date FROM orders) as o1 ON FALSE WHERE EXISTS (SELECT order_date);
SELECT 1 FROM customers c1 RIGHT JOIN (SELECT order_date FROM orders) as o1 ON FALSE WHERE EXISTS (SELECT order_date);
SELECT 1 FROM customers LEFT JOIN (SELECT order_date FROM orders WHERE order_date::varchar = 'abcd') as o1 ON TRUE WHERE EXISTS (SELECT order_date);
SELECT 1 FROM customers LEFT JOIN (SELECT order_date FROM orders GROUP BY order_date HAVING order_date::varchar = 'abcd') as o1 ON TRUE WHERE EXISTS (SELECT order_date);
SELECT ALL 1 FROM customers  LEFT JOIN (SELECT alias4.order_date FROM orders alias4 GROUP BY alias4.order_date HAVING alias4.order_date::varchar LIKE '$mxu') AS alias4 ON (customers.last_name != alias4.order_date::varchar) WHERE NOT EXISTS (SELECT 1 FROM customers alias6 ORDER BY alias4.order_date);
SELECT ALL 1 FROM customers  LEFT JOIN (SELECT alias4.order_date FROM orders alias4 GROUP BY alias4.order_date HAVING alias4.order_date::varchar LIKE '$_mxu_') AS alias4 ON (customers.last_name != alias4.order_date::varchar) WHERE NOT EXISTS (SELECT 1 FROM customers alias6 ORDER BY LEAST(alias4.order_date,alias4.order_date));
SELECT ALL first_name FROM customers  LEFT JOIN (SELECT alias4.order_date,alias4.customer_id FROM orders alias4 GROUP BY alias4.order_date,alias4.customer_id HAVING alias4.order_date::varchar LIKE '05/23/1784') AS alias4 ON (customers.customer_id = alias4.customer_id) WHERE EXISTS (SELECT 1 FROM customers alias6 ORDER BY alias4.order_date);

SELECT 1 FROM customers c1 RIGHT JOIN (SELECT order_date FROM orders) as o1 ON FALSE WHERE EXISTS (SELECT order_date);
SELECT 1 FROM customers c1 RIGHT JOIN (SELECT order_date FROM orders) as o1 ON FALSE WHERE EXISTS (SELECT order_date);
SELECT 1 FROM customers RIGHT JOIN (SELECT order_date FROM orders WHERE order_date::varchar = 'abcd') as o1 ON TRUE WHERE EXISTS (SELECT order_date);
SELECT 1 FROM customers RIGHT JOIN (SELECT order_date FROM orders GROUP BY order_date HAVING order_date::varchar = 'abcd') as o1 ON TRUE WHERE EXISTS (SELECT order_date);
SELECT ALL 1 FROM customers  RIGHT JOIN (SELECT alias4.order_date FROM orders alias4 GROUP BY alias4.order_date HAVING alias4.order_date::varchar LIKE '$mxu') AS alias4 ON (customers.last_name != alias4.order_date::varchar) WHERE NOT EXISTS (SELECT 1 FROM customers alias6 ORDER BY alias4.order_date);
SELECT ALL 1 FROM customers  RIGHT JOIN (SELECT alias4.order_date FROM orders alias4 GROUP BY alias4.order_date HAVING alias4.order_date::varchar LIKE '$_mxu_') AS alias4 ON (customers.last_name != alias4.order_date::varchar) WHERE NOT EXISTS (SELECT 1 FROM customers alias6 ORDER BY LEAST(alias4.order_date,alias4.order_date));
SELECT ALL first_name FROM customers  RIGHT JOIN (SELECT alias4.order_date,alias4.customer_id FROM orders alias4 GROUP BY alias4.order_date,alias4.customer_id HAVING alias4.order_date::varchar LIKE '1784-05-23') AS alias4 ON (customers.customer_id = alias4.customer_id) WHERE EXISTS (SELECT 1 FROM customers alias6 ORDER BY alias4.order_date);

