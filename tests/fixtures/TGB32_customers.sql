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

SELECT ALL first_name FROM customers c GROUP BY first_name HAVING EXISTS (SELECT 1 FROM orders GROUP BY order_id having c.first_name = 'James');
SELECT ALL first_name FROM customers c GROUP BY first_name HAVING EXISTS (SELECT 1 FROM orders n1 GROUP BY order_id having EXISTS(SELECT 1 from orders n2 group by n2.order_id having c.first_name != 'James'));
SELECT ALL first_name, EXISTS(SELECT 1 from orders n2 group by n2.order_id having c.first_name = 'James') from customers c group by first_name;
SELECT ALL first_name FROM customers c order by EXISTS (SELECT 1 FROM orders where c.last_name != 'James' group by order_id);
SELECT ALL first_name,count(customer_id) FROM customers c group by c.first_name order by EXISTS (SELECT 1 FROM orders where c.first_name != 'James' group by order_id); --sort-needed-check
SELECT ALL first_name,count(customer_id) FROM customers c group by c.first_name order by EXISTS (SELECT 1 FROM orders where c.first_name ='James' group by order_id); --sort-needed-check


SELECT ALL first_name FROM customers c GROUP BY first_name HAVING EXISTS (SELECT 1 FROM orders GROUP BY order_id having count(c.customer_id) > 1);
SELECT ALL first_name FROM customers c GROUP BY first_name HAVING EXISTS (SELECT 1 FROM orders n1 GROUP BY order_id having EXISTS(SELECT 1 from orders n2 group by n2.order_id having count(c.customer_id) > 1));
SELECT ALL first_name FROM customers c GROUP BY first_name HAVING EXISTS (SELECT 1 FROM orders n1 GROUP BY order_id having EXISTS(SELECT 1 from orders n2 group by n2.order_id having count(c.first_name) > 1));
SELECT ALL first_name FROM customers c GROUP BY first_name HAVING EXISTS (SELECT count(c.customer_id) FROM orders);

SELECT ALL 1 FROM customers c HAVING EXISTS (SELECT 1 FROM orders n1 GROUP BY order_id having EXISTS(SELECT 1 from orders n2 group by n2.order_id having count(c.first_name) > 1));

SELECT ALL first_name,count(customer_id) FROM customers c group by c.first_name having EXISTS (SELECT 1 FROM orders where count(c.first_name) > 1 group by order_id);
SELECT ALL first_name,count(customer_id),EXISTS (SELECT 1 FROM orders where count(c.first_name) > 1 group by order_id) FROM customers c group by c.first_name;

select count((select first_name from customers limit 1)) from customers;

SELECT ALL first_name FROM ((select first_name from customers c1) UNION ALL (select first_name from customers c2)) c GROUP BY first_name HAVING EXISTS (SELECT 1 FROM orders GROUP BY order_id having c.first_name = 'James');
SELECT ALL first_name FROM ((select first_name from customers c1) UNION ALL (select first_name from customers c2) UNION ALL (select first_name from customers c3)) c GROUP BY first_name HAVING EXISTS (SELECT 1 FROM orders GROUP BY order_id having c.first_name = 'James');
SELECT ALL first_name FROM ((select * from customers c1) UNION ALL (select * from customers c2) UNION ALL (select * from customers c3)) c GROUP BY first_name HAVING EXISTS (SELECT 1 FROM orders GROUP BY order_id having count(c.customer_id) > 1);

-- Grouped column reference in inner query
SELECT ALL first_name FROM customers c GROUP BY first_name HAVING EXISTS (SELECT 1 FROM orders GROUP BY order_id having count(c.first_name) > 1);
SELECT ALL first_name FROM customers c GROUP BY first_name HAVING EXISTS (SELECT 1 FROM orders GROUP BY order_id having c.first_name='James');

-- WHERE clause usage
SELECT ALL first_name FROM customers  LEFT JOIN (SELECT alias4.order_date,alias4.customer_id FROM orders alias4 GROUP BY alias4.order_date,alias4.customer_id HAVING alias4.order_date::varchar LIKE '1784-05-23') AS alias4 ON (customers.customer_id = alias4.customer_id) WHERE EXISTS (SELECT 1 FROM customers alias6 ORDER BY alias4.order_date);

-- VALUES usage
SELECT ALL c.column2 FROM (VALUES (1,'George'),(2,'John'),(3,'Thomas'),(4,'James'),(5,'James')) c GROUP BY c.column2 HAVING EXISTS (SELECT 1 FROM (VALUES (1),(2),(3),(4),(5),(6)) o GROUP BY o.column1 having count(c.column1) > 1);

-- SET operation usage
SELECT ALL c.column2 FROM ((select 1 as column1,'George' as column2) UNION (select 2 as column1,'John' as column2) UNION (select 3 as column1,'Thomas' as column2) UNION (select 4 as column1,'James' as column2) UNION (select 5 as column1,'James' as column2)) c GROUP BY c.column2 HAVING EXISTS (SELECT 1 FROM ((select 1 as column1) UNION (select 2 as column1) UNION (select 3 as column1) UNION (select 4 as column1) UNION (select 5 as column1) UNION (select 6 as column1)) o GROUP BY o.column1 having count(c.column1) > 1);

SELECT ALL first_name, EXISTS(SELECT 1 from orders n2 group by n2.order_id having count(c.last_name) > 1) from customers c group by first_name;
SELECT ALL first_name from customers c group by first_name order by EXISTS(SELECT 1 from orders n2 group by n2.order_id having count(c.last_name) > 1); --sort-needed-check
SELECT ALL first_name, EXISTS(SELECT 1 from orders n2 group by n2.order_id having c.first_name = 'James') from customers c group by first_name;

-- #806
SELECT ALL first_name,count(customer_id) FROM customers c group by c.first_name having EXISTS (SELECT 1 FROM orders where count(c.first_name) > 1 group by order_id);
SELECT ALL first_name,count(customer_id) FROM customers c group by c.first_name order by EXISTS (SELECT 1 FROM orders where count(c.first_name) > 1 group by order_id); --sort-needed-check;
SELECT ALL first_name,count(customer_id),EXISTS (SELECT 1 FROM orders where count(c.first_name) > 1 group by order_id) FROM customers c group by c.first_name;

-- Column belonging to outer and inner query in WHERE
-- Single column
SELECT ALL 1 FROM customers c ORDER BY EXISTS (SELECT 1 FROM customers n1 WHERE count(c.last_name)=1);
SELECT ALL 1 FROM customers c WHERE EXISTS (SELECT 1 FROM customers n1 ORDER BY count(n1.last_name)=1);

-- Expression with all columns belonging to the same query
SELECT ALL 1 FROM customers c ORDER BY EXISTS (SELECT 1 FROM customers n1 WHERE count(c.last_name=c.first_name)=1);
SELECT ALL 1 FROM customers c WHERE EXISTS (SELECT 1 FROM customers n1 ORDER BY count(n1.last_name=n1.first_name)=1);

-- Expression with columns belonging to query executing WHERE and a query not executing WHERE
SELECT ALL 1 FROM customers c WHERE EXISTS (SELECT 1 FROM customers n1 ORDER BY count(c.last_name=n1.first_name)=1);

SELECT ALL 1 FROM customers c GROUP BY first_name HAVING EXISTS (SELECT 1 FROM customers n1 where count(c.first_name=c.first_name)=1 GROUP BY n1.last_name);

SELECT ALL first_name FROM customers c GROUP BY first_name HAVING EXISTS (SELECT 1 FROM customers n1 where EXISTS(SELECT 1 from orders n2 group by n2.order_id having ((c.first_name=n1.first_name))));
SELECT ALL first_name FROM customers c GROUP BY first_name HAVING EXISTS (SELECT 1 FROM customers n1 GROUP BY n1.first_name having EXISTS(SELECT 1 from orders n2 group by n2.order_id having ((c.first_name=n1.first_name))));
SELECT ALL first_name FROM customers c GROUP BY first_name HAVING EXISTS (SELECT 1 FROM customers n1 GROUP BY n1.first_name having EXISTS(SELECT 1 from orders n2 group by n2.order_id having ((c.first_name=n1.first_name))));
SELECT ALL first_name FROM customers c GROUP BY first_name HAVING EXISTS (SELECT 1 FROM customers n1 GROUP BY n1.first_name having EXISTS(SELECT 1 from orders n2 group by n2.order_id having ((c.first_name||n1.first_name)=c.first_name)));
SELECT ALL first_name FROM customers c GROUP BY first_name HAVING EXISTS (SELECT 1 FROM customers n1 where EXISTS(SELECT 1 from orders n2 group by n2.order_id having ((c.first_name=n1.first_name))) GROUP BY n1.first_name);

-- Aggregates having subquery for its parameter
SELECT ALL 1 FROM customers c having EXISTS (SELECT 1 FROM customers n1 GROUP BY n1.last_name having count((select count(1) from customers n3 limit 1))=1);
SELECT ALL 1 FROM customers c having EXISTS (SELECT 1 FROM customers n1 GROUP BY n1.last_name having count((select count(n3.first_name) from customers n3 limit 1))=1);
SELECT ALL 1 FROM customers c having EXISTS (SELECT 1 FROM customers n1 GROUP BY n1.last_name having count((select count(n1.last_name||n3.first_name) from customers n3 limit 1))=1);
SELECT ALL 1 FROM customers c group by last_name having EXISTS (SELECT 1 FROM customers n1 GROUP BY n1.last_name having count((select count(c.last_name||n3.first_name) from customers n3 limit 1))=1);

-- Misc
SELECT ALL first_name FROM customers c GROUP BY first_name HAVING EXISTS (SELECT 1 FROM customers n1 left join (select last_name from customers c2 group by last_name)c2 on n1.first_name=c2.last_name order by EXISTS(SELECT 1 from orders n2 group by n2.order_id having ((c.first_name=n1.first_name))));
