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

select customer_id - 3 from orders group by customer_id - 3;
select customer_id + 3 from orders group by customer_id + 3;
select customer_id + 3, customer_id - 3 from orders group by customer_id + 3, customer_id - 3;
-- Having
SELECT ALL (1 * orders.customer_id) + (orders.customer_id + 3) + (4 + orders.order_id) - (1 - orders.order_id) + (3 - orders.order_id) - (orders.customer_id + 3) FROM orders  INNER JOIN customers AS alias1 ON ((orders.order_id <= alias1.customer_id) AND ((orders.order_amount != ANY (SELECT alias1.order_date FROM orders alias1 WHERE alias1.order_amount IN ('$78.50', '$78.50', '$78.50') ORDER BY alias1.order_date LIMIT 1)))) WHERE EXISTS (SELECT DISTINCT alias3.first_name, alias3.city, alias3.email FROM customers alias3 WHERE alias3.first_name IN ('George', 'John', 'James')) GROUP BY (1 * orders.customer_id) + (orders.customer_id + 3) + (4 + orders.order_id) - (1 - orders.order_id) + (3 - orders.order_id) - (orders.customer_id + 3) HAVING ((((1 * orders.customer_id) + (orders.customer_id + 3) + (4 + orders.order_id) - (1 - orders.order_id) + (3 - orders.order_id) - (orders.customer_id + 3)) >= 3));

select (1 != orders.order_id) or (10 <= orders.customer_id) FROM orders  INNER JOIN orders AS alias1 ON (((orders.order_amount < '$124.00'))) WHERE ((orders.customer_id-3)=10) GROUP BY (1 != orders.order_id) OR (10 <= orders.customer_id) HAVING (((1 != orders.order_id) or (10 <= orders.customer_id)) <= TRUE);

-- Below queries check if expression usage in inner query referring to column of outer query works (Related to #766)
SELECT ALL NOT (3 < customers.customer_id) FROM customers  WHERE EXISTS (SELECT MIN(ALL alias4.order_amount) as order_amount, alias4.order_id FROM orders alias4 WHERE ((alias4.order_date||'03/14/1760')<='07/21/179507/04/1776') GROUP BY alias4.order_id, NOT (3 < customers.customer_id)) GROUP BY customers.customer_id;

SELECT ALL NOT (3 < customers.customer_id) FROM customers  WHERE EXISTS (SELECT MIN(ALL alias4.order_amount) as order_amount, alias4.order_id FROM orders alias4 WHERE ((alias4.order_date||'03/14/1760')<='07/21/179507/04/1776') GROUP BY alias4.order_id HAVING (NOT (3 < customers.customer_id)=TRUE)) GROUP BY customers.customer_id;

SELECT ALL customers.first_name||'x' FROM customers  WHERE EXISTS (SELECT MIN(ALL alias4.order_amount) as order_amount, alias4.order_id FROM orders alias4 WHERE ((alias4.order_date||'03/14/1760')<='07/21/179507/04/1776') GROUP BY alias4.order_id, customers.first_name||'x') Group BY customers.first_name;

SELECT ALL NOT (3 < customers.customer_id) FROM customers  WHERE EXISTS (SELECT MIN(ALL alias4.order_amount) as order_amount, alias4.order_id FROM orders alias4 WHERE ((alias4.order_date||'03/14/1760')<='07/21/179507/04/1776') GROUP BY alias4.order_id, NOT (3 < customers.customer_id));

SELECT ALL customers.customer_id FROM customers  WHERE EXISTS (SELECT MIN(ALL alias4.order_amount) as order_amount, alias4.order_id FROM orders alias4 WHERE ((alias4.order_date||'03/14/1760')<='07/21/179507/04/1776') GROUP BY alias4.order_id, customers.customer_id+1 having (customers.customer_id+1)=2) GROUP BY customers.customer_id;

SELECT ALL customers.customer_id FROM customers  WHERE EXISTS (SELECT MIN(ALL alias4.order_amount) as order_amount, alias4.order_id FROM orders alias4 WHERE ((alias4.order_date||'03/14/1760')<='07/21/179507/04/1776') GROUP BY alias4.order_id, customers.customer_id+1 having (customers.customer_id+1)=2);

SELECT ALL NOT (3 < customers.customer_id) FROM customers  WHERE EXISTS (SELECT MIN(ALL alias4.order_amount) as order_amount, alias4.order_id FROM orders alias4 WHERE ((alias4.order_date||'03/14/1760')<='07/21/179507/04/1776') GROUP BY alias4.order_id having (NOT (3 < customers.customer_id))=TRUE) GROUP BY customers.customer_id;

-- The fix for literals to parameters problem with GROUP BY expressions is validated by running the following two queries in conjunction
--   At present `customers.first_name||'x'` in GROUP BY is retained during qualify_query() call which is not true for
--   a simple column usage `customers.first_name`. In case of the expression if a constant is present like 'x' and if the
--   expression gets removed in qualify_query() because the column in the expression is referring to outer query and acts
--   as a constant the parameter number of the query will now not represent the right correspondence between the literal
--   and parameter as one of it is removed but the numbering remain the same. To avoid this issue which hasn't got an easy
--   fix the expression though referring to the outer query column is retained in qualify_query() invocation.
SELECT ALL customers.first_name||'x' FROM customers  WHERE EXISTS (SELECT MIN(ALL alias4.order_amount) as order_amount, alias4.order_id FROM orders alias4 WHERE ((alias4.order_date||'03/14/1760')<='07/21/179507/04/1776') GROUP BY alias4.order_id, customers.first_name||'x' having (customers.first_name||'x')='Georgex');

SELECT ALL customers.first_name||'x' FROM customers  WHERE EXISTS (SELECT MIN(ALL alias4.order_amount) as order_amount, alias4.order_id FROM orders alias4 WHERE ((alias4.order_date||'03/14/1760')<='07/21/179507/04/1776') GROUP BY alias4.order_id having (customers.first_name||'x')='Georgex');

SELECT ALL customers.first_name||'x' FROM customers  WHERE EXISTS (SELECT MIN(ALL alias4.order_amount) as order_amount, alias4.order_id FROM orders alias4 WHERE ((alias4.order_date||'03/14/1760')<='07/21/179507/04/1776') GROUP BY alias4.order_id, customers.first_name||'x' having (customers.first_name||'x')='Georgex') Group by customers.first_name;

SELECT ALL customers.first_name||'x' FROM customers  WHERE EXISTS (SELECT MIN(ALL alias4.order_amount) as order_amount, alias4.order_id FROM orders alias4 WHERE ((alias4.order_date||'03/14/1760')<='07/21/179507/04/1776') GROUP BY alias4.order_id, customers.first_name||'x' having (customers.first_name||'x')='Georgex') Group by customers.first_name||'x';

SELECT ALL customers.first_name||'x' FROM customers  WHERE EXISTS (SELECT MIN(ALL alias4.order_amount) as order_amount, alias4.order_id FROM orders alias4 WHERE ((alias4.order_date||'03/14/1760')<='07/21/179507/04/1776') GROUP BY alias4.order_id, customers.first_name having (customers.first_name||'x')='Georgex');

SELECT ALL customers.first_name||'x' FROM customers  WHERE EXISTS (SELECT MIN(ALL alias4.order_amount) as order_amount, alias4.order_id FROM orders alias4 WHERE ((alias4.order_date||'03/14/1760')<='07/21/179507/04/1776') GROUP BY alias4.order_id having (customers.first_name||'x')='Georgex') GROUP BY customers.first_name||'x';

SELECT ALL customers.first_name||'x' FROM customers  WHERE EXISTS (SELECT MIN(ALL alias4.order_amount) as order_amount, alias4.order_id FROM orders alias4 WHERE ((alias4.order_date||'03/14/1760')<='07/21/179507/04/1776') GROUP BY alias4.order_id having (customers.first_name||'x')='Georgex') GROUP BY customers.first_name;

-- Following tests IS NULL usage with OrderBy and GroupBY
SELECT (('05/23/1784' || (alias1.order_date))) IS NULL FROM customers  INNER JOIN (SELECT DISTINCT alias1.order_date FROM orders alias1 WHERE alias1.customer_id IN (3, 2, 3, 3) ORDER BY alias1.order_date) AS alias1 ON ((customers.email != ALL (SELECT ALL alias2.state FROM customers alias2 WHERE alias2.last_name BETWEEN 'Jefferson' AND 'Jefferson' ORDER BY alias2.state LIMIT 1)) OR NOT ((customers.first_name >= alias1.order_date))) LEFT JOIN orders AS alias4 ON ((('Charlottesville' != '$234.56')) AND ((customers.last_name = SOME (SELECT alias4.state FROM customers alias4 WHERE (('Madison' >= 'Washington')) ORDER BY alias4.state LIMIT 1))) AND ('Charlottesville' = '$234.56')) LEFT JOIN (SELECT MIN(DISTINCT alias6.address) as address, alias6.first_name FROM customers alias6 WHERE ((alias6.last_name||alias6.first_name)>'MadisonThomas') GROUP BY alias6.first_name ORDER BY MIN(DISTINCT alias6.address), alias6.first_name) AS alias6 ON (((customers.zipcode = alias6.first_name)) AND NOT (customers.last_name = ANY (SELECT DISTINCT alias7.order_date FROM orders alias7 WHERE alias7.order_amount = ANY (SELECT alias8.order_date FROM orders alias8 WHERE ((alias8.order_date||3)='03/14/1760') ORDER BY alias8.order_date LIMIT 1) ORDER BY alias7.order_date LIMIT 1)) OR ((customers.address > ANY (SELECT ALL alias9.zipcode FROM customers alias9 WHERE alias9.customer_id IN (2, 4, 1) ORDER BY alias9.zipcode LIMIT 1)))) GROUP BY 1 ORDER BY (('05/23/1784' || (alias1.order_date))) IS NULL;

-- Following test is from https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/977#note_863296655 related to expression and #800 fix
SELECT ALL 1 FROM customers  LEFT JOIN (SELECT alias4.order_date FROM orders alias4 GROUP BY alias4.order_date HAVING alias4.order_date LIKE '$_mxu_') AS alias4 ON (customers.last_name != alias4.order_date) WHERE NOT EXISTS (SELECT 1 FROM customers alias6 GROUP BY LEAST(alias4.order_date,alias4.order_date));
