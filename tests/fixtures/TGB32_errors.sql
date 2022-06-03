#################################################################
#								#
# Copyright (c) 2020-2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- Outer query column reference in WHERE clause of sub query when parent has GROUP BY usage
-- (ERROR reference to ungrouped column)
SELECT ALL first_name,count(customer_id) FROM customers c group by c.first_name order by EXISTS (SELECT 1 FROM orders where c.last_name != 'James' group by order_id);

select first_name from customers c join (select n2.order_date,n2.customer_id from orders n2 group by c.first_name) as n2 on (c.customer_id = n2.customer_id) group by first_name;

SELECT ALL first_name FROM customers  LEFT JOIN (SELECT alias4.order_date,alias4.customer_id FROM orders alias4 GROUP BY alias4.order_date,alias4.customer_id HAVING alias4.order_date LIKE '05/23/1784') AS alias4 ON (customers.customer_id = alias4.customer_id) WHERE EXISTS (SELECT 1 FROM customers alias6 ORDER BY count(alias4.order_date));

SELECT ALL first_name FROM customers  LEFT JOIN (SELECT alias4.customer_id FROM orders alias4 GROUP BY alias4.order_date,alias4.customer_id HAVING alias4.order_date LIKE '05/23/1784') AS alias4 ON (customers.customer_id = alias4.customer_id) WHERE EXISTS (SELECT 1 FROM customers alias6 ORDER BY alias4.order_date);

SELECT ALL first_name, EXISTS(SELECT 1 from orders n2 group by n2.order_id having c.last_name = 'James') from customers c group by first_name;

-- Aggregates having subquery for its parameter
SELECT ALL 1 FROM customers c having EXISTS (SELECT 1 FROM customers n1 GROUP BY n1.last_name having count((select count(c.first_name||n3.first_name) from customers n3 limit 1))=1);
SELECT ALL 1 FROM customers c having EXISTS (SELECT 1 FROM customers n1 GROUP BY n1.last_name having count((select count(n1.last_name) from customers n3 limit 1))=1);
SELECT ALL 1 FROM customers c having EXISTS (SELECT 1 FROM customers n1 GROUP BY n1.last_name having count((select count(n1.last_name||'abc') from customers n3 limit 1))=1);
SELECT ALL 1 FROM customers c having EXISTS (SELECT 1 FROM customers n1 GROUP BY n1.last_name having count((select count(n1.first_name) from customers n3 limit 1))=1);
SELECT ALL 1 FROM customers c having EXISTS (SELECT 1 FROM customers n1 GROUP BY n1.last_name having count((select count(c.first_name) from customers n3 limit 1))=1);
SELECT ALL 1 FROM customers c having EXISTS (SELECT 1 FROM customers n1 GROUP BY n1.last_name having count((select count(n1.last_name) from customers n3 limit 1))=1);
SELECT ALL 1 FROM customers c having EXISTS (SELECT 1 FROM customers n1 GROUP BY n1.last_name having count((select count(n1.last_name||'abc') from customers n3 limit 1))=1);
SELECT ALL 1 FROM customers c having EXISTS (SELECT 1 FROM customers n1 GROUP BY n1.last_name having count((select count(c.last_name) from customers n3 limit 1))=1);
SELECT ALL 1 FROM customers c having EXISTS (SELECT 1 FROM customers n1 GROUP BY n1.last_name having count((select count(c.last_name||'abc') from customers n3 limit 1))=1);

-- #806
SELECT ALL first_name,count(customer_id) from customers c where count(c.first_name)>1 group by c.first_name;
SELECT ALL first_name,count(customer_id),EXISTS (SELECT 1 FROM orders where count(order_id) > 1 group by order_id) FROM customers c group by c.first_name;

-- #819
select firstname from names n1 ORDER BY EXISTS (select count(n1.firstname));
select firstname from names n1 ORDER BY count(n1.firstname);

-- Column belonging to outer and inner query in WHERE
-- Single column
SELECT ALL 1 FROM customers c ORDER BY EXISTS (SELECT 1 FROM customers n1 WHERE count(n1.first_name)=1);
SELECT ALL 1 FROM customers c ORDER BY EXISTS (SELECT 1 FROM customers n1 WHERE count(n1.last_name)=1);
SELECT ALL 1 FROM customers c WHERE EXISTS (SELECT 1 FROM customers n1 ORDER BY count(c.first_name)=1);
SELECT ALL 1 FROM customers c WHERE EXISTS (SELECT 1 FROM customers n1 ORDER BY count(c.last_name)=1);

-- Expression with all columns belonging to the same query
SELECT ALL 1 FROM customers c ORDER BY EXISTS (SELECT 1 FROM customers n1 WHERE count(n1.first_name=n1.last_name)=1);
SELECT ALL 1 FROM customers c ORDER BY EXISTS (SELECT 1 FROM customers n1 WHERE count(n1.last_name=n1.first_name)=1);
SELECT ALL 1 FROM customers c WHERE EXISTS (SELECT 1 FROM customers n1 ORDER BY count(c.first_name=c.last_name)=1);
SELECT ALL 1 FROM customers c WHERE EXISTS (SELECT 1 FROM customers n1 ORDER BY count(c.last_name=c.first_name)=1);

-- Expression with columns belonging to query executing WHERE and a query not executing WHERE
SELECT ALL 1 FROM customers c ORDER BY EXISTS (SELECT 1 FROM customers n1 WHERE count(c.last_name=n1.first_name)=1);

-- #820
select (select lastname) from names group by firstname;

